
# Why it is impossible to divide Integer number in Haskell?

## Question
        
This code

    (4 :: Integer) / 2
    

will lead to error:

      No instance for (Fractional Integer) arising from a use of ‘/’
        In the expression: (4 :: Integer) / 2
        In an equation for ‘it’: it = (4 :: Integer) / 2
    

Why?

I need to specify

    fromIntegral(4 :: Integer) / 2
    

to get a result. But what if I need a real number and not `2.0`?

## Answer
        
Because the `Integer` type has no `Fractional` instance.

The type for `(/)` is `Fractional a => a -> a -> a`. Consider what happens when `a = Integer`. You'd have `Integer -> Integer -> Integer`. But `1/2` is **not** an integer, it's `0.5`. So the only way to fit the division operator would be to _round_ the result. But there is _not_ a single way to round, and the best choice depends on the application, hence it was decided to _not_ provide that instance.

If you want to perform _integer division_ use the [`div`](https://hackage.haskell.org/package/base-4.8.2.0/docs/Prelude.html#v:div) or [`quot`](https://hackage.haskell.org/package/base-4.8.2.0/docs/Prelude.html#v:quot) functions (they use different rounding). Otherwise convert to something that supports a well-defined division operation like `Rational` (this is what the `fromIntegral` is doing).
