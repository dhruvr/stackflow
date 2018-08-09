
# Haskell: Overlapping instances

## Question
        
Consider the following example program:

    next :: Int -> Int
    next i
      | 0 == m2 = d2
      | otherwise = 3 * i + 1
      where
        (d2, m2) = i `divMod` 2
    
    loopIteration :: MaybeT (StateT Int IO) ()
    loopIteration = do
      i <- get
      guard $ i > 1
      liftIO $ print i
      modify next
    
    main :: IO ()
    main = do
      (`runStateT` 31) . runMaybeT . forever $ loopIteration
      return ()
    

It can only use `get` instead of `lift get` because `instance MonadState s m => MonadState s (MaybeT m)` is defined in the MaybeT module.

Many such instances are defined in kind of a combinatoric explosion manner.

It would have been nice (although impossible? why?) if we had the following type-class:

    {-# LANGUAGE MultiParamTypeClasses #-}
    
    class SuperMonad m s where
      lifts :: m a -> s a
    

Let's try to define it as such:

    {-# LANGUAGE FlexibleInstances, ... #-}
    
    instance SuperMonad a a where
      lifts = id
    
    instance (SuperMonad a b, MonadTrans t, Monad b) => SuperMonad a (t b) where
      lifts = lift . lifts
    

Using `lifts $ print i` instead of `liftIO $ print i` works, which is nice.

But using `lifts (get :: StateT Int IO Int)` instead of `(get :: MaybeT (StateT Int IO) Int)` doesn't work.

GHC (6.10.3) gives the following error:

    Overlapping instances for SuperMonad
                                (StateT Int IO) (StateT Int IO)
      arising from a use of `lifts'
    Matching instances:
      instance SuperMonad a a
      instance (SuperMonad a b, MonadTrans t, Monad b) =>
               SuperMonad a (t b)
    In a stmt of a 'do' expression:
        i <- lifts (get :: StateT Int IO Int)
    

I can see why "`instance SuperMonad a a`" applies. But why does GHC think that the other one does, too?

## Answer
        
To follow up ephemient's excellent answer: Haskell type classes use an _open-world assumption_: some idiot can come along later and add an instance declaration that's _not a duplicate_ and yet _overlaps with_ your instance. **Think of it as an adversary game**: if an adversary can make your program ambiguous, the compiler bleats.

If you're using GHC you can of course say to the compiler "to hell with your paranoia; allow me my ambiguous instance declaration":

    {-# LANGUAGE OverlappingInstances #-}
    

If later evolution of your program leads to overload resolution you didn't expect, the compiler gets 1,000 I-told-you-so points :-)
