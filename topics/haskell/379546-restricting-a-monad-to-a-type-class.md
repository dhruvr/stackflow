
# Restricting a monad to a type class

## Question
        
In Haskell, is there a way to restrict a monad `M a` so that `a` satisfy a type class constraint?

I am translating the [probabilistic modeling example](http://github.com/namin/spots/tree/master/probabilisticModeling/README.markdown) from [F#](http://github.com/namin/spots/tree/master/probabilisticModeling/probabilisticModeling.fsx) to [Haskell](http://github.com/namin/spots/tree/6c5a3b78e8f5f559900bade7629fab0edcf225e8/probabilisticModeling/probabilisticModeling.hs). However, in Haskell, I omitted `support` because it would change `data Distribution a` to `data (Ord a) => Distribution a`. With this change, I get the following error:

    ...probabilisticModeling.hs:42:13:
        Could not deduce (Ord a) from the context ()
          arising from a use of `always'
                       at ...probabilisticModeling.hs:42:13-18
        Possible fix:
          add (Ord a) to the context of the type signature for `return'
        In the expression: always
        In the definition of `return': return = always
        In the instance declaration for `Monad Distribution'
    

Indeed, the type of `always`/`return` is: `(Ord a) => a -> Distribution a`. Is there a way I can have a monad `Distribution`, but force the constraint `(Ord a)` on this monad? I tried:

    instance Monad Distribution where
        (>>=) = bind
        return :: (Ord a) => a -> Distribution a = always
    

But I get the error:

    ...probabilisticModeling2.hs:48:4:
        Pattern bindings (except simple variables) not allowed in instance declarations
          return :: (Ord a) => a -> Distribution a = always
    Failed, modules loaded: none.
    

So it there a way to have a monad `M a`, but restrict the `a` with a constraint such as `Ord a`?

Thanks.

## Answer
        
My understanding of this is that you simply cannot, because a monad is meant to be generalized over all types, not some restricted subset of types such as `(Ord a)`.

Instead of restricting the monadic type `M a`, you can simply restrict functions which use that monadic type, e.g.,

foo :: Ord a => Int -> M a

In fact, it is preferable to keep types as general as possible and use type classes only to restrict functions.

etc.
