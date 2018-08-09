
# Integral operators quot vs. div

## Question
        
Type class Integral has two operations `quot` and `div`, yet in the Haskell 2010 Language Report it is not specified what they're supposed to do. Assuming that `div` is integral division, what does `quot` differently, or what is the purpose of `quot`? When do you use one, and when the other?

## Answer
        
To quote section 6.4.2 from the Haskell report:

The `quot`, `rem`, `div`, and `mod` class methods satisfy these laws if y is non-zero:

    (x `quot` y)*y + (x `rem` y) == x  
    (x `div`  y)*y + (x `mod` y) == x
    

`quot` is integer division truncated toward zero, while the result of `div` is truncated toward negative infinity.

The `div` function is often the more natural one to use, whereas the `quot` function corresponds to the machine instruction on modern machines, so it's somewhat more efficient.
