
# Return specific type within Haskell

## Question
        
I have a pretty general question about Haskell's type system. I'm trying to become familiar with it, and I have the following function:

    getN :: Num a => a
    getN = 5.0 :: Double
    

When I try to compile this, I get the following error:

    Couldn't match expected type `a' against inferred type `Double'
      `a' is a rigid type variable bound by
          the type signature for `getN' at Perlin.hs:15:12
    In the expression: 5.0 :: Double
    In the definition of `getN': getN = 5.0 :: Double
    

As I understand this, the function is set up to "return" a type in the class Num. Double is in this class ([http://www.zvon.org/other/haskell/Outputprelude/Num_c.html](http://www.zvon.org/other/haskell/Outputprelude/Num_c.html)), so I would have expected that it would be okay to "return" a Double in this case.

Can someone explain this please?

## Answer
        
A function with signature `Num a => a` is expected to work for _any_ type in the class `Num`. The implementation `5.0 :: Double` just works for _one_ type, not for _all_ types of the class, so the compiler complains.

An example of a generic function would be:

    square :: (Num a) => a -> a
    square x = x * x
    

This works for _any_ type that is a `Num`. It works for doubles, integers and whatever other numbers you want to use. Because of that it can have a generic type signature that just requires the parameter to be in class `Num`. (Type class `Num` is necessary because the function uses multiplication with `*`, which is defined there)
