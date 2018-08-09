
# When is the difference between quotRem and divMod useful?

## Question
        
From the haskell report:

> The quot, rem, div, and mod class methods satisfy these laws if y is non-zero:
> 
>     (x `quot` y)*y + (x `rem` y) == x
>     (x `div`  y)*y + (x `mod` y) == x
>     
> 
> `quot` is integer division truncated toward zero, while the result of `div` is truncated toward negative infinity.

For example:

    Prelude> (-12) `quot` 5
    -2
    Prelude> (-12) `div` 5
    -3
    

What are some examples of where the difference between how the result is truncated matters?

## Answer
        
Many languages have a "mod" or "%" operator that gives the remainder after division with truncation towards 0; for example C, C++, and Java, and probably C#, would say:

    (-11)/5 = -2
    (-11)%5 = -1
    5*((-11)/5) + (-11)%5 = 5*(-2) + (-1) = -11.
    

Haskell's `quot` and `rem` are intended to imitate this behaviour. I can imagine compatibility with the output of some C program might be desirable in some contrived situation.

Haskell's `div` and `mod`, and subsequently Python's / and %, follow the convention of mathematicians (at least number-theorists) in always truncating _down_ division (not towards 0 -- towards negative infinity) so that the remainder is always nonnegative. Thus in Python,

    (-11)/5 = -3
    (-11)%5 = 4
    5*((-11)/5) + (-11)%5 = 5*(-3) + 4 = -11.
    

Haskell's `div` and `mod` follow this behaviour.
