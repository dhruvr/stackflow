
# Why does the 2-tuple Functor instance only apply the function to the second element?

## Question
        
    import Control.Applicative
    
    main = print $ fmap (*2) (1,2)
    

produces `(1,4)`. I would expect it it to produce `(2,4)` but instead the function is applied only to the second element of the tuple.

**Update** I've basically figured this out almost straight away. I'll post my own answer in a minute..

## Answer
        
The `Functor` instance is actually from the [GHC.Base](https://hackage.haskell.org/package/base-4.8.1.0/docs/src/GHC.Base.html#line-625) module which is imported by `Control.Applicative`.

Trying to write the instance I want, I can see that it won't work, given the definition of tuples; the instance requires just one type parameter, while the 2-tuple has two.

A valid `Functor` instance would at least have to be on tuples, `(a,a)` that have the same type for each element, but you cannot do anything sneaky, like define the instance on:

     type T2 a = (a,a)
    

because instance types aren't permitted to be synonyms.

The above restricted 2-tuple synonym is logically the same as the type:

    data T2 a = T2 a a
    

which _can_ have a Functor instance:

    instance Functor T2 where
        fmap f (T2 x y) = T2 (f x) (f y)
    

As Gabriel remarked in the comments, this can be useful for branching structures or concurrency.
