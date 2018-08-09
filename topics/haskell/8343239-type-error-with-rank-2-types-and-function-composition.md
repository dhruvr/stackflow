
# Type error with rank-2 types and function composition

## Question
        
Here are some pragmas and some imports:

    {-# LANGUAGE ScopedTypeVariables #-}
    
    import Control.Monad.ST
    import Data.Array.ST
    import Data.Array
    

Now here's my problem. The following code typechecks:

    foo :: forall a. a -> [a]
    foo x = elems $ runSTArray $ do
        newListArray (1,10) (replicate 10 x) :: ST s (STArray s Int a)
    

However, when I replace the `$` with composition:

    foo :: forall a. a -> [a]
    foo x = elems . runSTArray $ do
        newListArray (1,10) (replicate 10 x) :: ST s (STArray s Int a)
    

I get this error:

    Couldn't match expected type `forall s. ST s (STArray s i0 e0)'
                with actual type `ST s0 (STArray s0 Int a)'
    In the expression:
        newListArray (1, 10) (replicate 10 x) :: ST s (STArray s Int a)
    In the second argument of `($)', namely
      `do { newListArray (1, 10) (replicate 10 x) ::
              ST s (STArray s Int a) }'
    In the expression:
          elems . runSTArray
      $ do { newListArray (1, 10) (replicate 10 x) ::
               ST s (STArray s Int a) }
    

What's werid is, if I give the function composition its own name, then it typechecks again:

    elemSTArray = elems . runSTArray
    
    foo :: forall a. a -> [a]
    foo x = elemSTArray $ do
        newListArray (1,10) (replicate 10 x) :: ST s (STArray s Int a)
    

I'm not sure what's going on here. I would expect the second piece of code to typecheck nicely. And I don't understand why it typechecks again if I give the composed function its own name.

This is a simplified version of some code that I had that broke when upgrading from GHC 6.2 to 7 and I'm trying to understand why this happens now. Thanks for helping!

## Answer
        
As you already hint at in the title of your post, the problem has to do with `runSTArray` having a polymorphic type of rank 2.

    runSTArray :: Ix i => (forall s. ST s (STArray s i e)) -> Array i e
    

With

    elems :: Ix i => Array i e -> [e]
    

and

    ($) :: (a -> b) -> a -> b
    

writing `runSTArray $ ...` means that the type variable `a` in the type schema of `($)` needs to be instantiated with a polymorphic type rather than a monomorphic type. This requires so-called impredicative polymorphism. How GHC implements impredicative polymorphism is explained in the ICFP 2008 paper by Dimitrios Vytiniotis, Stephanie Weirich, and Simon Peyton Jones: [FPH : First-class Polymorphism for Haskell](http://research.microsoft.com/en-us/um/people/simonpj/papers/boxy/). The bottom line is that while FPH often gives you the behaviour that you expect, typeability is sometimes not preserved under simple transformations like the ones you describe in your question: see Section 6.2 of the aforementioned paper.
