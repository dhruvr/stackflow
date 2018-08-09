
# How do I build a list with a dependently-typed length?

## Question
        
Dipping my toe into the waters of dependent types, I had a crack at the canonical "list with statically-typed length" example.

    {-# LANGUAGE DataKinds, GADTs, KindSignatures #-}
    
    -- a kind declaration
    data Nat = Z | S Nat
    
    data SafeList :: (Nat -> * -> *) where
        Nil :: SafeList Z a
        Cons :: a -> SafeList n a -> SafeList (S n) a
    
    -- the type signature ensures that the input list has at least one element
    safeHead :: SafeList (S n) a -> a
    safeHead (Cons x xs) = x
    

This seems to work:

    ghci> :t Cons 5 (Cons 3 Nil)
    Cons 5 (Cons 3 Nil) :: Num a => SafeList ('S ('S 'Z)) a
    
    ghci> safeHead (Cons 'x' (Cons 'c' Nil))
    'x'
    
    ghci> safeHead Nil
    Couldn't match type 'Z with 'S n0
    Expected type: SafeList ('S n0) a0
      Actual type: SafeList 'Z a0
    In the first argument of `safeHead', namely `Nil'
    In the expression: safeHead Nil
    In an equation for `it': it = safeHead Nil
    

However, in order for this data-type to be actually useful, I should be able to build it from run-time data for which you don't know the length at compile time. My naïve attempt:

    fromList :: [a] -> SafeList n a
    fromList = foldr Cons Nil
    

This fails to compile, with the type error:

    Couldn't match type 'Z with 'S n
    Expected type: a -> SafeList n a -> SafeList n a
      Actual type: a -> SafeList n a -> SafeList ('S n) a
    In the first argument of `foldr', namely `Cons'
    In the expression: foldr Cons Nil
    In an equation for `fromList': fromList = foldr Cons Nil
    

I understand why this is happening: the return type of `Cons` is different for each iteration of the fold - that's the whole point! But I can't see a way around it, probably because I've not read deeply enough into the subject. (I can't imagine all this effort is being put into a type system that is impossible to use in practice!)

So: How can I build this sort of dependently-typed data from 'normal' simply-typed data?

* * *

Following @luqui's advice I was able to make `fromList` compile:

    data ASafeList a where
        ASafeList :: SafeList n a -> ASafeList a
    
    fromList :: [a] -> ASafeList a
    fromList = foldr f (ASafeList Nil)
        where f x (ASafeList xs) = ASafeList (Cons x xs)
    

Here's my attempt to unpack the `ASafeList` and use it:

    getSafeHead :: [a] -> a
    getSafeHead xs = case fromList xs of ASafeList ys -> safeHead ys
    

This causes another type error:

    Couldn't match type `n' with 'S n0
      `n' is a rigid type variable bound by
          a pattern with constructor
            ASafeList :: forall a (n :: Nat). SafeList n a -> ASafeList a,
          in a case alternative
          at SafeList.hs:33:22
    Expected type: SafeList ('S n0) a
      Actual type: SafeList n a
    In the first argument of `safeHead', namely `ys'
    In the expression: safeHead ys
    In a case alternative: ASafeList ys -> safeHead ys
    

Again, intuitively it makes sense that this would fail to compile. I can call `fromList` with an empty list, so the compiler has no guarantee that I'll be able to call `safeHead` on the resulting `SafeList`. This lack of knowledge is roughly what the existential `ASafeList` captures.

Can this problem be solved? I feel like I might have walked down a logical dead-end.

## Answer
        
Never throw anything away.

If you're going to take the trouble to crank along a list to make a length-indexed list (known in the literature as a "vector"), you may as well remember its length.

So, we have

    data Nat = Z | S Nat
    
    data Vec :: Nat -> * -> * where -- old habits die hard
      VNil :: Vec Z a
      VCons :: a -> Vec n a -> Vec (S n) a
    

but we can also give a run time representation to static lengths. Richard Eisenberg's "Singletons" package will do this for you, but the basic idea is to give a type of run time representations for static numbers.

    data Natty :: Nat -> * where
      Zy :: Natty Z
      Sy :: Natty n -> Natty (S n)
    

Crucially, if we have a value of type `Natty n`, then we can interrogate that value to find out what `n` is.

Hasochists know that run time representability is often so boring that even a machine can manage it, so we hide it inside a type class

    class NATTY (n :: Nat) where
      natty :: Natty n
    
    instance NATTY Z where
      natty = Zy
    
    instance NATTY n => NATTY (S n) where
      natty = Sy natty
    

Now we can give a slightly more informative existential treatment of the length you get from your lists.

    data LenList :: * -> * where
      LenList :: NATTY n => Vec n a -> LenList a
    
    lenList :: [a] -> LenList a
    lenList []        = LenList VNil
    lenList (x : xs)  = case lenList xs of LenList ys -> LenList (VCons x ys)
    

You get the same code as the length-destroying version, but you can grab a run time representation of the length anytime you like, and you don't need to crawl along the vector to get it.

Of course, if you want the length to be a `Nat`, it's still a pain that you instead have a `Natty n` for some `n`.

It's a mistake to clutter one's pockets.

**Edit** I thought I'd add a little, to address the "safe head" usage issue.

First, let me add an unpacker for `LenList` which gives you the number in your hand.

    unLenList :: LenList a -> (forall n. Natty n -> Vec n a -> t) -> t
    unLenList (LenList xs) k = k natty xs
    

And now suppose I define

    vhead :: Vec (S n) a -> a
    vhead (VCons a _) = a
    

enforcing the safety property. If I have a run time representation of the length of a vector, I can look at it to see if `vhead` applies.

    headOrBust :: LenList a -> Maybe a
    headOrBust lla = unLenList lla $ \ n xs -> case n of
      Zy    -> Nothing
      Sy _  -> Just (vhead xs)
    

So you look at one thing, and in doing so, learn about another.
