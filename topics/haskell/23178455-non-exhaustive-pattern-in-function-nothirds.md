
# Non exhaustive pattern in function noThirds

## Question
        
So, my Problem is, that I have to write a program that filters all 3 * x (3,6,9...) elements from a list. My program looks like:

    length'  :: [a] -> Int
    length' = foldr (\_ -> (+1)) 0
    
    help_ :: [a] -> [a] -> [a]
    help_ (x:xs) [] = help_ (xs) [x]
    help_ [] (x) = (x)
    help_ (x:xs) (y)
        |((length' [xs]) ==0) = (y)
        |((length' [y]) `mod` 2 ==0) = help_ (xs) (y)
        |otherwise = help_ (xs) (y++[x])
    
    noThirds :: [a] -> [a]
    noThirds [x] = help_ [x] []
    

The compiler accepts this but gives the error "Non exhaustive pattern in function noThirds" when I enter "noThirds \[1,2,3,4,5,6,7,8\]" . I guess it's cause im missing a variety of "help_ .." but I don't get it. Im grateful for every help! Btw predefined list and arithmetic functions are not allowed.

## Answer
        
It's because `noThirds` only has one pattern, `[x]` which only matches against a single element list. `[x]` is exactly equivalent to `(x : [])`. What I think you meant was

    noThirds :: [a] -> [a]
    noThirda xs = help_ xs []
