
# How does Data.MemoCombinators work?

## Question
        
I've been looking at the source for [Data.MemoCombinators](http://hackage.haskell.org/packages/archive/data-memocombinators/0.4.1/doc/html/Data-MemoCombinators.html) but I can't really see where the heart of it is.

Please explain to me what the logic is behind all of these combinators and _the mechanics_ of how they actually work to speed up your program in real world programming.

I'm looking for specifics for **this** implementation, and optionally comparison/contrast with other Haskell approaches to memoization. I understand what memoization is and am _not_ looking for a description of how it works in general.

## Answer
        
This library is a straightforward combinatorization of the well-known technique of memoization. Let's start with the canonical example:

    fib = (map fib' [0..] !!)
        where
        fib' 0 = 0
        fib' 1 = 1
        fib' n = fib (n-1) + fib (n-2)
    

I interpret what you said to mean that you know how and why this works. So I'll focus on the combinatorization.

We are essentiallly trying to capture and generalize the idea of `(map f [0..] !!)`. The type of this function is `(Int -> r) -> (Int -> r)`, which makes sense: it takes a function from `Int -> r` and returns a memoized version of the same function. Any function which is semantically the identity and has this type is called a "memoizer for `Int`" (even `id`, which doesn't memoize). We generalize to this abstraction:

    type Memo a = forall r. (a -> r) -> (a -> r)
    

So a `Memo a`, a memoizer for `a`, takes a function from `a` to anything, and returns a semantically identical function that has been memoized (or not).

The idea of the different memoizers is to find a way to enumerate the domain with a data structure, map the function over them, and then index the data structure. `bool` is a good example:

    bool :: Memo Bool
    bool f = table (f True, f False)
        where
        table (t,f) True = t
        table (t,f) False = f
    

Functions from `Bool` are equivalent to pairs, except a pair will only evaluate each component once (as is the case for every value that occurs outside a lambda). So we just map to a pair and back. The essential point is that we are lifting the evaluation of the function above the lambda for the argument (here the last argument of `table`) by enumerating the domain.

Memoizing `Maybe a` is a similar story, except now we need to know how to memoize `a` for the `Just` case. So the memoizer for `Maybe` takes a memoizer for `a` as an argument:

    maybe :: Memo a -> Memo (Maybe a)
    maybe ma f = table (f Nothing, ma (f . Just))
        where
        table (n,j) Nothing = n
        table (n,j) (Just x) = j x
    

The rest of the library is just variations on this theme.

The way it memoizes integral types uses a more appropriate structure than `[0..]`. It's a bit involved, but basically just creates an infinite tree (representing the numbers in binary to elucidate the structure):

    1
      10
        100
          1000
          1001
        101
          1010
          1011
      11
        110
          1100
          1101
        111
          1110
          1111
    

So that looking up a number in the tree has running time proportional to the number of bits in its representation.

As sclv points out, Conal's MemoTrie library uses the same underlying technique, but uses a typeclass presentation instead of a combinator presentation. We released our libraries independently at the same time (indeed, within a couple hours!). Conal's is easier to use in simple cases (there is only one function, `memo`, and it will determine the memo structure to use based on the type), whereas mine is more flexible, as you can do things like this:

    boundedMemo :: Integer -> Memo Integer
    boundedMemo bound f = \z -> if z < bound then memof z else f z
       where
       memof = integral f
    

Which only memoizes values less than a given bound, needed for the implementation of one of the project euler problems.

There are other approaches, for example exposing an open fixpoint function over a monad:

    memo :: MonadState ... m => ((Integer -> m r) -> (Integer -> m r)) -> m (Integer -> m r)
    

Which allows yet more flexibility, eg. purging caches, LRU, etc. But it is a pain in the ass to use, and also it puts strictness constraints on the function to be memoized (e.g. no infinite left recursion). I don't believe there are any libraries that implement this technique.

Did that answer what you were curious about? If not, perhaps make explicit the points you are confused about?
