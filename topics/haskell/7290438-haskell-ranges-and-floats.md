
# Haskell ranges and floats

## Question
      
Why is the behavior of the Haskell range notation different for floats than for integers and chars?

    Prelude> [1, 3 .. 10] :: [Int]
    [1,3,5,7,9] 
    Prelude> [1, 3 .. 10] :: [Float]
    [1.0,3.0,5.0,7.0,9.0,11.0]
    Prelude> ['a', 'c' .. 'f']
    "ace"
    

I would understand it if the last element was close to the upper bound, but this is obviously not a rounding issue.
## Answer
      
The syntax `[e1, e2 .. e3]` is really syntactic sugar for `enumFromThenTo e1 e2 e3`, which is a function in the `Enum` typeclass.

[The Haskell standard](http://www.haskell.org/onlinereport/haskell2010/haskellch6.html#x13-1310006.3.4) defines its semantics as follows:

> For the types `Int` and `Integer`, the enumeration functions have the following meaning:
> 
> *   The sequence `enumFrom e1` is the list `[e1,e1 + 1,e1 + 2,…]`.
> *   The sequence `enumFromThen e1 e2` is the list `[e1,e1 + i,e1 + 2i,…]`, where the increment, `i`, is `e2 − e1`. The increment may be zero or negative. If the increment is zero, all the list elements are the same.
> *   The sequence `enumFromTo e1 e3` is the list `[e1,e1 + 1,e1 + 2,…e3]`. The list is empty if `e1 > e3`.
> *   The sequence `enumFromThenTo e1 e2 e3` is the list `[e1,e1 + i,e1 + 2i,…e3]`, where the increment, `i`, is `e2 − e1`. If the increment is positive or zero, the list terminates when the next element would be greater than `e3`; the list is empty if `e1 > e3`. If the increment is negative, the list terminates when the next element would be less than `e3`; the list is empty if `e1 < e3`.

This is pretty much what you'd expect, but the `Float` and `Double` instances are defined differently:

> For `Float` and `Double`, the semantics of the `enumFrom` family is given by the rules for `Int` above, except that the list terminates when the elements become greater than `e3 + i∕2` for positive increment `i`, or when they become less than `e3 + i∕2` for negative `i`.

I'm not really sure what the justification for this is, so the only answer I can give you is that it is that way because it's defined that way in the standard.

You can work around this by enumerating using integers and converting to `Float` afterward.

    Prelude> map fromIntegral [1, 3 .. 10] :: [Float]
    [1.0,3.0,5.0,7.0,9.0]
    