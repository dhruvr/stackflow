
# How to reuse a type variable in an inner type declaration

## Question
        
As part of my Haskell learning process, I like to explicitly type out the type declarations for functions. I would like to be able to do so for functions defined in a where clause, but I don't know how to specify, that a type variable in a where clause should denote the same type as some type variable in the outer type declaration. For instance, the following code:

    foo :: (a -> a) -> a -> a
    foo f arg = bar arg
      where
        bar :: a -> a
        bar a = f a
    

yields this error:

    src\Test.hs:7:14:
        Couldn't match expected type `a' against inferred type `a1'
          `a' is a rigid type variable bound by
              the type signature for `foo' at src\Test.hs:3:8
          `a1' is a rigid type variable bound by
               the type signature for `bar' at src\Test.hs:6:11
        In the first argument of `f', namely `a'
        In the expression: f a
        In the definition of `bar': bar a = f a
    

How can I express that the first argument to bar should be of the same type as the second argument to foo, so that I can apply f to it?

Thanks.

## Answer
        
I think you can do this in general with [ScopedTypeVariables](http://www.haskell.org/ghc/docs/latest/html/users_guide/other-type-extensions.html#scoped-type-variables) which GHC supports. This certainly compiles:

    {-# LANGUAGE ScopedTypeVariables #-}
    foo :: forall a. (a -> a) -> a -> a
    foo f arg = bar arg
      where
        bar :: a -> a
        bar a = f a
    

Note the "forall a."
