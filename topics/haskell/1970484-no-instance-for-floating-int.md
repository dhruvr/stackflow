
# No instance for (Floating Int)

## Question
        
I am learning Haskell. I have created function which returns multiplication table up to 'n' in base 'b'. Numbers are padded to 'w' digits. As the last step, I want to compute 'w' automatically. Why does this not compile?

    -- Number of digits needed for the multiplication table n*n in base 'base'
    nOfDg :: Int -> Int-> Int 
    nOfDg n base = 1 + floor ( logBase base (n*n)) 
    

error:

    No instance for (Floating Int)
         arising from a use of `logBase' at C:\haskel\dgnum.hs:4:24-38
       Possible fix: add an instance declaration for (Floating Int)
       In the first argument of `floor', namely `(logBase b (n * n))'
       In the second argument of `(+)', namely `floor (logBase b (n * n))'
       In the expression: 1 + floor (logBase b (n * n))

## Answer
        
logBase takes two parameters which implement the floating typeclass. You'll need to call fromIntegral on the parameters before passing them to logBase. This compiled for me with 6.10.3:

    nOfDg :: Int -> Int-> Int
    nOfDg n base = 1 + floor ( logBase (fromIntegral base) (fromIntegral (n*n)))
    

You have to remember that Haskell is very strongly typed, so you can't just assume that the Int parameters supplied to your function will automatically be coerced to the floating numbers that log functions generally take.
