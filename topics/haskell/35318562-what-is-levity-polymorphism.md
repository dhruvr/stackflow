
# What is Levity polymorphism

## Question
        
As the title of the question indicates, I want to know what is Levity polymorphism and what is its motivation ? I know [this page](https://ghc.haskell.org/trac/ghc/wiki/NoSubKinds) has some details in it, but most of the explanations there go over the top of my head. :)

While [this page](http://ezyang.tumblr.com/post/127984263867/richard-eisenberg-levity-polymorphism-hiw) is a little friendlier, I'm still not able to understand the motivation behind it.

## Answer
        
**Note:** This answer is based on very recent observations on Levity discussions. Everything concerning Levity polymorphism is currently only implemented in the GHC 8.0 release candidates and as such subject to change (see [#11471](https://ghc.haskell.org/trac/ghc/ticket/11471) for example).

* * *

**TL;DR**: It's a way to make functions polymorphic over lifted and unlifted types, which is not possible with regular functions. For example the following code doesn't type check with regular poly­mor­phi­sms, since `Int#` has kind `#`, but the type variables in `id` have kind `*`:

    {-# LANGUAGE MagicHash #-}
    
    import GHC.Prim
    
    example :: Int# -> Int# 
    example = id            -- does not work, since id :: a -> a
    

    Couldn't match kind ‘*’ with ‘#’
    When matching types
      a0 :: *
      Int# :: #
    Expected type: Int# -> Int#
      Actual type: a0 -> a0
    In the expression: id
    

Note that `(->)` still uses some magic.

* * *

Before I start to answer this question, let us take a step back and go to one of the most often used functions, `($)`.

What is `($)`'s type? Well, according to Hackage and the report, it's

    ($) :: (a -> b) -> a -> b
    

However, that's not 100% complete. It's a convenient little lie. The problem is that polymorphic types (like `a` and `b`) have kind `*`. However, (library) developers wanted to use `($)` not only for types with kind `*`, but also for those of kind `#`, e.g.

    unwrapInt :: Int -> Int#
    

While `Int` has kind `*` (it can be bottom), `Int#` has kind `#` (and cannot be bottom at all). Still, the following code typechecks:

    unwrapInt $ 42
    

That shouldn't work. Remember the return type of `($)`? It was polymorphic, and polymorphic types have kind `*`, not `#`! So why did it work? First, it was [a bug](https://ghc.haskell.org/trac/ghc/ticket/8739), and then it was a [hack](https://github.com/ghc/ghc/commit/5dd1cbbfc0a19e92d7eeff6f328abc7558992fd6) (excerpt of [a mail by Ryan Scott](https://mail.haskell.org/pipermail/ghc-devs/2016-February/011269.html) on the ghc-dev mailing list):

> So why is this happening?
> 
> The long answer is that prior to GHC 8.0, in the type signature `($) :: (a -> b) -> a -> b`, `b` actually wasn't in kind `*`, but rather `OpenKind`. `OpenKind` is an awful hack that allows both lifted (kind `*`) and unlifted (kind `#`) types to inhabit it, which is why `(unwrapInt $ 42)` typechecks.

So what is `($)`'s new type in GHC 8.0? It's

    ($) :: forall (w :: Levity) a (b :: TYPE w). (a -> b) -> a -> b
    -- will likely change according to Richard E.
    

To understand it, we must look at `Levity`:

    data Levity = Lifted | Unlifted
    

Now, we can think of `($)` as having either one of the following types, since there are only two choices of `w`:

    -- pseudo types
    ($) :: forall a (b :: TYPE   Lifted). (a -> b) -> a -> b
    ($) :: forall a (b :: TYPE Unlifted). (a -> b) -> a -> b
    

`TYPE` is a magical constant, and it redefines the kinds `*` and `#` as

    type * = TYPE Lifted
    type # = TYPE Unlifted
    

The quantification over kinds [is also fairly new](https://ghc.haskell.org/trac/ghc/wiki/DependentHaskell) and part of the [integration of dependent types in Haskell](https://ghc.haskell.org/trac/ghc/wiki/DependentHaskell/Phase1).

The name _Levity polymorphism_ comes from the fact that you now can write polymorphic functions over both lifted and unlifted types, something that wasn't allowed/possible with the previous poly­mor­phism restrictions. It also gets rid of the `OpenKind` hack at the same time. It's really "just" about this, handling both kinds of kinds.

By the way, you're not alone with your question. Even [Simon Peyton Jones said that there's a need for a Levity wiki page](https://ghc.haskell.org/trac/ghc/ticket/11471#comment:7), and Richard E. (the current implementer of this) [stated that the wiki page needs an update](https://ghc.haskell.org/trac/ghc/ticket/11471#comment:8) on the current process.

### References

*   [Levity Polymorphism In Dependent Haskell](https://www.youtube.com/watch?v=bDdkeKr9vVw); talk by Richard A. Eisenberg on ICFP 2015. Very recommended.
*   [Levity Polymorphism (extended version)](https://www.microsoft.com/en-us/research/wp-content/uploads/2016/11/levity-1.pdf) by Richard A. Eisenberg and Simon Peyton Jones
*   [`GHC.Types`](https://github.com/ghc/ghc/blob/master/libraries/ghc-prim/GHC/Types.hs), part of the `ghc-prim` library that's shipped with GHC.
*   Discussions:
    *   The discussion on [ghc-dev](https://mail.haskell.org/pipermail/ghc-devs/2016-February/011268.html).
    *   The discussion on [haskell-cafe](https://mail.haskell.org/pipermail/haskell-cafe/2016-February/122914.html).
