
# Incomplete type signature

## Question
        
Lets say we've got a function like f below, that returns a monad. However, where you see `Int`, pretend it's a really complicated type.

    f :: (Monad m) => m Int -- Pretend this isn't Int but something complicated
    f = return 42
    

Now lets say we want to force this into the `Maybe` monad. We don't need to write the full type of `f` to do this, we can just do the following:

    g :: Maybe a -> Maybe a
    g = id
    
    main = print $ (g f)
    

The dummy function `g` forces `f` to become `Maybe`.

I think the above is rather messy. What I'd rather write is this:

    main = print $ (f :: Maybe a)
    

But it fails with the following error:

    Couldn't match expected type `a' against inferred type `Int'
      `a' is a rigid type variable bound by
          the polymorphic type `forall a. Maybe a' at prog.hs:7:16
      Expected type: Maybe a
      Inferred type: Maybe Int
    In the second argument of `($)', namely `(f :: Maybe a)'
    In the expression: print $ (f :: Maybe a)
    

Is there a way to do what `g` above does in a less messy way that doesn't involve creating a new function? I don't want to write `f :: Maybe Int`, as it becomes a maintenance problem if the return type changes. GHC extensions are okay in answers.

## Answer
        
Use [`asTypeOf`](http://www.haskell.org/ghc/docs/latest/html/libraries/base/Prelude.html#v:asTypeOf). It returns the first argument while unifying its type with that of the second. It's just a type-restricted version of `const`, but useful for situations like this.

    main = print $ f `asTypeOf` (undefined :: Maybe a)
