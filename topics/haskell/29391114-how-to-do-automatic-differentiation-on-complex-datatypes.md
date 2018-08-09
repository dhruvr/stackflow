
# How to do automatic differentiation on complex datatypes?

## Question
        
Given a very simple Matrix definition based on Vector:

    import Numeric.AD
    import qualified Data.Vector as V
    
    newtype Mat a = Mat { unMat :: V.Vector a }
    
    scale' f = Mat . V.map (*f) . unMat
    add' a b = Mat $ V.zipWith (+) (unMat a) (unMat b)
    sub' a b = Mat $ V.zipWith (-) (unMat a) (unMat b)
    mul' a b = Mat $ V.zipWith (*) (unMat a) (unMat b)
    pow' a e = Mat $ V.map (^e) (unMat a)
    
    sumElems' :: Num a => Mat a -> a
    sumElems' = V.sum . unMat
    

(for demonstration purposes ... I am using hmatrix but thought the problem was there somehow)

And an error function (`eq3`):

    eq1' :: Num a => [a] -> [Mat a] -> Mat a
    eq1' as φs = foldl1 add' $ zipWith scale' as φs
    
    eq3' :: Num a => Mat a -> [a] -> [Mat a] -> a
    eq3' img as φs = negate $ sumElems' (errImg `pow'` (2::Int))
      where errImg = img `sub'` (eq1' as φs)
    

Why the compiler not able to deduce the right types in this?

    diffTest :: forall a . (Fractional a, Ord a) => Mat a -> [Mat a] -> [a] -> [[a]]
    diffTest m φs as0 = gradientDescent go as0
      where go xs = eq3' m xs φs
    

The exact error message is this:

    src/Stuff.hs:59:37:
        Could not deduce (a ~ Numeric.AD.Internal.Reverse.Reverse s a)
        from the context (Fractional a, Ord a)
          bound by the type signature for
                     diffTest :: (Fractional a, Ord a) =>
                                 Mat a -> [Mat a] -> [a] -> [[a]]
          at src/Stuff.hs:58:13-69
        or from (reflection-1.5.1.2:Data.Reflection.Reifies
                   s Numeric.AD.Internal.Reverse.Tape)
          bound by a type expected by the context:
                     reflection-1.5.1.2:Data.Reflection.Reifies
                       s Numeric.AD.Internal.Reverse.Tape =>
                     [Numeric.AD.Internal.Reverse.Reverse s a]
                     -> Numeric.AD.Internal.Reverse.Reverse s a
          at src/Stuff.hs:59:21-42
          ‘a’ is a rigid type variable bound by
              the type signature for
                diffTest :: (Fractional a, Ord a) =>
                            Mat a -> [Mat a] -> [a] -> [[a]]
              at src//Stuff.hs:58:13
        Expected type: [Numeric.AD.Internal.Reverse.Reverse s a]
                       -> Numeric.AD.Internal.Reverse.Reverse s a
          Actual type: [a] -> a
        Relevant bindings include
          go :: [a] -> a (bound at src/Stuff.hs:60:9)
          as0 :: [a] (bound at src/Stuff.hs:59:15)
          φs :: [Mat a] (bound at src/Stuff.hs:59:12)
          m :: Mat a (bound at src/Stuff.hs:59:10)
          diffTest :: Mat a -> [Mat a] -> [a] -> [[a]]
            (bound at src/Stuff.hs:59:1)
        In the first argument of ‘gradientDescent’, namely ‘go’
        In the expression: gradientDescent go as0

## Answer
        
The [`gradientDescent`](http://haddocks.fpcomplete.com/fp/7.8/20140916-162/ad/Numeric-AD-Newton.html#v:gradientDescent) function from [`ad`](https://hackage.haskell.org/package/ad) has the type

    gradientDescent :: (Traversable f, Fractional a, Ord a) =>
                       (forall s. Reifies s Tape => f (Reverse s a) -> Reverse s a) ->
                       f a -> [f a]
    

Its first argument requires a function of the type `f r -> r` where `r` is `forall s. (Reverse s a)`. `go` has the type `[a] -> a` where `a` is the type bound in the signature of `diffTest`. These `a`s are the same, but `Reverse s a` isn't the same as `a`.

The [`Reverse`](https://hackage.haskell.org/package/ad-4.2.1.1/docs/Numeric-AD-Mode-Reverse.html#t:Reverse) type has instances for a number of type classes that could allow us to convert an `a` into a `Reverse s a` or back. The most obvious is `Fractional a => Fractional (Reverse s a)` which would allow us to convert `a`s into `Reverse s a`s with `realToFrac`.

To do so, we'll need to be able to map a function `a -> b` over a `Mat a` to obtain a `Mat b`. The easiest way to do this will be to derive a `Functor` instance for `Mat`.

    {-# LANGUAGE DeriveFunctor #-}
    
    newtype Mat a = Mat { unMat :: V.Vector a }
        deriving Functor
    

We can convert the `m` and `fs` into any `Fractional a' => Mat a'` with `fmap realToFrac`.

    diffTest m fs as0 = gradientDescent go as0
      where go xs = eq3' (fmap realToFrac m) xs (fmap (fmap realToFrac) fs)
    

But there's a better way hiding in the ad package. The `Reverse s a` is universally qualified over all `s` but the `a` is the same `a` as the one bound in the type signature for `diffTest`. We really only need a function `a -> (forall s. Reverse s a)`. This function is [`auto`](https://hackage.haskell.org/package/ad-4.2.1.1/docs/Numeric-AD-Mode.html#v:auto) from the `Mode` class, for which `Reverse s a` has an instance. `auto` has the slightly wierd type `Mode t => Scalar t -> t` but `type Scalar (Reverse s a) = a`. Specialized for `Reverse` `auto` has the type

    auto :: (Reifies s Tape, Num a) => a -> Reverse s a
    

This allows us to convert our `Mat a`s into `Mat (Reverse s a)`s without messing around with conversions to and from `Rational`.

    {-# LANGUAGE ScopedTypeVariables #-}
    {-# LANGUAGE TypeFamilies #-}
    
    diffTest :: forall a . (Fractional a, Ord a) => Mat a -> [Mat a] -> [a] -> [[a]]
    diffTest m fs as0 = gradientDescent go as0
      where
        go :: forall t. (Scalar t ~ a, Mode t) => [t] -> t
        go xs = eq3' (fmap auto m) xs (fmap (fmap auto) fs)
