
# runST and function composition

## Question
      
Why does this typecheck:

    runST $ return $ True
    

While the following does not:

    runST . return $ True
    

GHCI complains:

    Couldn't match expected type `forall s. ST s c0'
                with actual type `m0 a0'
    Expected type: a0 -> forall s. ST s c0
      Actual type: a0 -> m0 a0
    In the second argument of `(.)', namely `return'
    In the expression: runST . return
## Answer
      
The short answer is that type inference doesn't always work with higher-rank types. In this case, it is unable to infer the type of `(.)`, but it type checks if we add an explicit type annotation:

    > :m + Control.Monad.ST
    > :set -XRankNTypes
    > :t (((.) :: ((forall s0. ST s0 a) -> a) -> (a -> forall s1. ST s1 a) -> a -> a) runST return) $ True
    (((.) :: ((forall s0. ST s0 a) -> a) -> (a -> forall s1. ST s1 a) -> a -> a) runST return) $ True :: Bool
    

The same problem also happens with your first example, if we replace `($)` with our own version:

    > let app f x = f x
    > :t runST `app` (return `app` True)
    <interactive>:1:14:
        Couldn't match expected type `forall s. ST s t0'
                    with actual type `m0 t10'
        Expected type: t10 -> forall s. ST s t0
          Actual type: t10 -> m0 t10
        In the first argument of `app', namely `return'
        In the second argument of `app', namely `(return `app` True)'
    

Again, this can be solved by adding type annotations:

    > :t (app :: ((forall s0. ST s0 a) -> a) -> (forall s1. ST s1 a) -> a) runST (return `app` True)
    (app :: ((forall s0. ST s0 a) -> a) -> (forall s1. ST s1 a) -> a) runST (return `app` True) :: Bool
    

What is happening here is that there is a special typing rule in GHC 7 which only applies to the standard `($)` operator. Simon Peyton-Jones explains this behavior in [a reply on the GHC users mailing list](http://www.mail-archive.com/glasgow-haskell-users@haskell.org/msg18923.html):

> This is a motivating example for type inference that can deal with impredicative types. Consider the type of `($)`:
> 
>     ($) :: forall p q. (p -> q) -> p -> q
>     
> 
> In the example we need to instantiate `p` with `(forall s. ST s a)`, and that's what impredicative polymorphism means: instantiating a type variable with a polymorphic type.
> 
> Sadly, I know of no system of reasonable complexity that can typecheck \[this\] unaided. There are plenty of complicated systems, and I have been a co-author on papers on at least two, but they are all Too Jolly Complicated to live in GHC. We did have an implementation of boxy types, but I took it out when implementing the new typechecker. Nobody understood it.
> 
> However, people so often write
> 
>     runST $ do ... 
>     
> 
> that in GHC 7 I implemented a special typing rule, just for infix uses of `($)`. Just think of `(f $ x)` as a new syntactic form, with the obvious typing rule, and away you go.

Your second example fails because there is no such rule for `(.)`.
    