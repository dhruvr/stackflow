
# How much is applicative really about applying, rather than &#x201C;combining&#x201D;?

## Question
        
For an [uncertainty-propagating `Approximate` type](https://github.com/leftaroundabout/uncertainly-haskell/blob/master/Data/Uncertain.hs), I'd like to have instances for `Functor` through `Monad`. This however doesn't work because I need a vector space structure on the contained types, so it must actually be restricted versions of the classes. As there still doesn't seem to be a standard library for those (or is there? please point me. There's [rmonad](http://hackage.haskell.org/package/rmonad), but it uses `*` rather than `Constraint` as the context kind, which seems just outdated to me), I wrote [my own version](https://github.com/leftaroundabout/constrained-categories) for the time being.

It all works easy for `Functor`

    class CFunctor f where
      type CFunctorCtxt f a :: Constraint
      cfmap :: (CFunctorCtxt f a, CFunctorCtxt f b)  => (a -> b) -> f a -> f b
    
    instance CFunctor Approximate where
      type CFunctorCtxt Approximate a = FScalarBasisSpace a
      f `cfmap` Approximate v us = Approximate v' us'
       where v' = f v
             us' = ...
    

but a direct translation of `Applicative`, like

    class CFunctor f => CApplicative' f where
      type CApplicative'Ctxt f a :: Constraint
      cpure' :: (CApplicative'Ctxt f a) => a -> f a
      (#<*>#) :: ( CApplicative'Ctxt f a
                 , CApplicative'Ctxt f (a->b)
                 , CApplicative'Ctxt f b)        => f(a->b) -> f a -> f b
    

is not possible because functions `a->b` do not have the necessary vector space structure* `FScalarBasisSpace`.

What does work, however, is to change the definition of the restricted applicative class:

    class CFunctor f => CApplicative f where
      type CApplicativeCtxt f a :: Constraint
      cpure :: CAppFunctorCtxt f a  => a -> f a
      cliftA2 :: ( CAppFunctorCtxt f a
                 , CAppFunctorCtxt f b
                 , CAppFunctorCtxt f c )        => (a->b->c) -> f a -> f b -> f c
    

and then defining `<*>#` rather than `cliftA2` as a free function

    (<*>#) = cliftA2 ($)
    

instead of a method. Without the constraint, that's completely equivalent (in fact, [many `Applicative` instances go this way anyway](https://stackoverflow.com/questions/5358868/how-and-why-is-ap-defined-as-liftm2-id-in-haskell)), but in this case it's actually better: `(<*>#)` still has the constraint on `a->b` which `Approximate` can't fulfill, but that doesn't hurt the applicative instance, and I can still do useful stuff like

    ghci> cliftA2 (\x y -> (x+y)/x^2) (3±0.2) (5±0.3)        :: Approximate Double 
    0.8888888888888888 +/- 0.10301238090045711
    

I reckon the situation would essentially the same for many other uses of `CApplicative`, for instance the `Set` example that's already given in the [original blog post on constraint kinds](http://blog.omega-prime.co.uk/?p=127).

So my question:
---------------

is `<*>` more fundamental than `liftA2`?

Again, in the unconstrained case they're equivalent anyway. I actually have found `liftA2` easier to understand, but in Haskell it's probably just more natural to think about passing "containers of functions" rather than containers of objects and some "global" operation to combine them. And `<*>` directly induces all the `liftAμ` for _μ_ ∊ ℕ, not just `liftA2`; doing that from `liftA2` only [doesn't really work](https://github.com/leftaroundabout/constrained-categories/blob/master/Control/Applicative/Constrained.hs#L77).

But then, these constrained classes seem to make quite a point for `liftA2`. In particular, it allows `CApplicative` instances for all `CMonad`s, which does _not_ work when `<*>#` is the base method. And I think we all agree that `Applicative` should always be more general than `Monad`.

What would the category theorists say to all of this? And is there a way to get the general `liftAμ` without `a->b` needing to fulfill the associated constraint?

* * *

*_Linear functions_ of that type actually do have the vector space structure, but I definitely can't restrict myself to those.

## Answer
        
As I understand it (as a non---category theorist), the fundamental operation is `zip :: f a -> f b -> f (a, b)` (mapping a pair of effectful computations to an effectful computation resulting in a pair).

You can then define

*   `fx <*> fy = uncurry ($) <$> zip fx fy`
*   `liftA2 g fx fy = uncurry g <$> zip fx fy`

See this [post by Edward Yang](http://blog.ezyang.com/2012/08/applicative-functors/), which I found [via the Typeclassopedia](http://www.haskell.org/haskellwiki/Typeclassopedia#Alternative_formulation).
