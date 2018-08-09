
# What&#x2019;s an example of a Monad which is an Alternative but not a MonadPlus?

## Question
        
In [his answer](https://stackoverflow.com/a/10168111/237428) to the question [“Distinction between typeclasses `MonadPlus`, `Alternative`, and `Monoid`?”](https://stackoverflow.com/q/10167879/237428), Edward Kmett says that

> Moreover, even if `Applicative` was a superclass of `Monad`, you’d wind up needing the `MonadPlus` class anyways, because obeying
> 
>     empty <*> m = empty
>     
> 
> isn’t strictly enough to prove that
> 
>     empty >>= f = empty
>     
> 
> So claiming that something is a `MonadPlus` is stronger than claiming it is `Alternative`.

It’s clear that any applicative functor which is _not_ a monad is automatically an example of an `Alternative` which is not a `MonadPlus`, but Edward Kmett’s answer implies that there exists a _monad_ which is an `Alternative` but not a `MonadPlus`: its `empty` and `<|>` would satisfy the `Alternative` laws,1 but not the `MonadPlus` laws.2 I can’t come up with an example of this by myself; does anybody know of one?

* * *

1 I wasn’t able to find a canonical reference for a set of `Alternative` laws, but I lay out what I believe them to be roughly halfway through [my answer](https://stackoverflow.com/a/13081604/237428) to the question [“Confused by the meaning of the `Alternative` type class and its relationship to other type classes”](https://stackoverflow.com/q/13080606/237428) (search for the phrase “right distributivity”). The four laws I believe ought to hold are:

1.  **Right distributivity (of `<*>`):**  `(f <|> g) <*> a = (f <*> a) <|> (g <*> a)`
2.  **Right absorption (for `<*>`):**  `empty <*> a = empty`
3.  **Left distributivity (of `fmap`):**  `f <$> (a <|> b) = (f <$> a) <|> (f <$> b)`
4.  **Left absorption (for `fmap`):**  `f <$> empty = empty`

I’d also happily accept being given a more useful set of `Alternative` laws.

2 I know that [there’s some ambiguity about what the `MonadPlus` laws are](http://www.haskell.org/haskellwiki/MonadPlus); I’m happy with an answer that uses left distribution or left catch, although I would weakly prefer the former.

## Answer
        
The clue to your answer is in the [HaskellWiki about MonadPlus you linked to](http://www.haskell.org/haskellwiki/MonadPlus):

> Which rules? Martin & Gibbons choose Monoid, Left Zero, and Left Distribution. This makes `[]` a MonadPlus, but not `Maybe` or `IO`.

So according to your favoured choice, `Maybe` isn't a MonadPlus (although there's an instance, it doesn't satisfy left distribution). Let's prove it satisfies Alternative.

`Maybe` is an Alternative
-------------------------

1.  **Right distributivity (of `<*>`):** `(f <|> g) <*> a = (f <*> a) <|> (g <*> a)`

Case 1: `f=Nothing`:

    (Nothing <|> g) <*> a =                    (g) <*> a  -- left identity <|>
                          = Nothing         <|> (g <*> a) -- left identity <|>
                          = (Nothing <*> a) <|> (g <*> a) -- left failure <*>
    

Case 2: `a=Nothing`:

    (f <|> g) <*> Nothing = Nothing                             -- right failure <*>
                          = Nothing <|> Nothing                 -- left identity <|>
                          = (f <*> Nothing) <|> (g <*> Nothing) -- right failure <*>
    

Case 3: `f=Just h, a = Just x`

    (Just h <|> g) <*> Just x = Just h <*> Just x                      -- left bias <|>
                              = Just (h x)                             -- success <*>
                              = Just (h x) <|> (g <*> Just x)          -- left bias <|>
                              = (Just h <*> Just x) <|> (g <*> Just x) -- success <*>
    

1.  **Right absorption (for `<*>`):** `empty <*> a = empty`

That's easy, because

    Nothing <*> a = Nothing    -- left failure <*>
    

1.  **Left distributivity (of `fmap`):** `f <$> (a <|> b) = (f <$> a) <|> (f <$> b)`

Case 1: `a = Nothing`

    f <$> (Nothing <|> b) = f <$> b                        -- left identity <|>
                     = Nothing <|> (f <$> b)          -- left identity <|>
                     = (f <$> Nothing) <|> (f <$> b)  -- failure <$>
    

Case 2: `a = Just x`

    f <$> (Just x <|> b) = f <$> Just x                 -- left bias <|>
                         = Just (f x)                   -- success <$>
                         = Just (f x) <|> (f <$> b)     -- left bias <|>
                         = (f <$> Just x) <|> (f <$> b) -- success <$>
    

1.  **Left absorption (for `fmap`):** `f <$> empty = empty`

Another easy one:

    f <$> Nothing = Nothing   -- failure <$>
    

`Maybe` isn't a MonadPlus
-------------------------

Let's prove the assertion that `Maybe` isn't a MonadPlus: We need to show that `mplus a b >>= k = mplus (a >>= k) (b >>= k)` doesn't always hold. The trick is, as ever, to use some binding to sneak very different values out:

    a = Just False
    b = Just True
    
    k True = Just "Made it!"
    k False = Nothing
    

Now

    mplus (Just False) (Just True) >>= k = Just False >>= k
                                         = k False
                                         = Nothing
    

here I've used bind `(>>=)` to snatch failure (`Nothing`) from the jaws of victory because `Just False` looked like success.

    mplus (Just False >>= k) (Just True >>= k) = mplus (k False) (k True)
                                               = mplus Nothing (Just "Made it!")
                                               = Just "Made it!"
    

Here the failure (`k False`) was calculated early, so got ignored and we `"Made it!"`.

So, `mplus a b >>= k = Nothing` but `mplus (a >>= k) (b >>= k) = Just "Made it!"`.

You can look at this as me using `>>=` to break the left-bias of `mplus` for `Maybe`.

Validity of my proofs:
----------------------

Just in case you felt I hadn't done enough tedious deriving, I'll prove the identities I used:

Firstly

    Nothing <|> c = c      -- left identity <|>
    Just d <|> c = Just d  -- left bias <|>
    

which come from the instance declaration

    instance Alternative Maybe where
        empty = Nothing
        Nothing <|> r = r
        l       <|> _ = l
    

Secondly

    f <$> Nothing = Nothing    -- failure <$>
    f <$> Just x = Just (f x)  -- success <$>
    

which just come from `(<$>) = fmap` and

    instance  Functor Maybe  where
        fmap _ Nothing       = Nothing
        fmap f (Just a)      = Just (f a)
    

Thirdly, the other three take a bit more work:

    Nothing <*> c = Nothing        -- left failure <*>
    c <*> Nothing = Nothing        -- right failure <*>
    Just f <*> Just x = Just (f x) -- success <*>
    

Which comes from the definitions

    instance Applicative Maybe where
        pure = return
        (<*>) = ap
    
    ap :: (Monad m) => m (a -> b) -> m a -> m b
    ap =  liftM2 id
    
    liftM2  :: (Monad m) => (a1 -> a2 -> r) -> m a1 -> m a2 -> m r
    liftM2 f m1 m2          = do { x1 <- m1; x2 <- m2; return (f x1 x2) }
    
    instance  Monad Maybe  where
        (Just x) >>= k      = k x
        Nothing  >>= _      = Nothing
        return              = Just
    

so

    mf <*> mx = ap mf mx
              = liftM2 id mf mx
              = do { f <- mf; x <- mx; return (id f x) }
              = do { f <- mf; x <- mx; return (f x) }
              = do { f <- mf; x <- mx; Just (f x) }
              = mf >>= \f ->
                mx >>= \x ->
                Just (f x)
    

so if `mf` or `mx` are Nothing, the result is also `Nothing`, whereas if `mf = Just f` and `mx = Just x`, the result is `Just (f x)`
