
# In Haskell, why non-exhaustive patterns are not compile-time errors?

## Question
        
This is a follow-up of [Why am I getting "Non-exhaustive patterns in function..." when I invoke my Haskell substring function?](https://stackoverflow.com/questions/3799359/why-am-i-getting-non-exhaustive-patterns-in-function-when-i-invoke-my-haskel)

I understand that using `-Wall`, GHC can warn against non-exhaustive patterns. I'm wondering what's the reason behind not making it a compile-time error by default given that it's always possible to explicitly define a partial function:

    f :: [a] -> [b] -> Int
    f [] _  = error "undefined for empty array"
    f _ []  = error "undefined for empty array"
    f (_:xs) (_:ys) = length xs + length ys
    

The question is not GHC-specific.

Is it because...

*   nobody wanted to enforce a Haskell compiler to perform this kind of analysis?
*   a non-exhaustive pattern search can find some but not all cases?
*   partially defined functions are considered legitimate and used often enough not to impose the kind of construct shown above? If this is the case, can you explain to me why non-exhaustive patterns are helpful/legitimate?

## Answer
        
There are cases where you don't mind that a pattern match is non-exhaustive. For example, while this might not be the optimal implementation, I don't think it would help if it didn't compile:

    fac 0 = 1
    fac n | n > 0 = n * fac (n-1)
    

That this is non-exhaustive (negative numbers don't match any case) doesn't really matter for the typical usage of the factorial function.

Also it might not generally be possible to decide for the compiler if a pattern match is exhaustive:

    mod2 :: Integer -> Integer
    mod2 n | even n = 0
    mod2 n | odd n  = 1
    

Here all cases should be covered, but the compiler probably can't detect it. Since the guards could be arbitrarily complex, the compiler cannot always decide if the patterns are exhaustive. Of course this example would better be written with `otherwise`, but I think it should also compile in its current form.
