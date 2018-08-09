
# How would you define map and filter using foldr in Haskell?

## Question
        
I'm doing a bit of self study on functional languages (currently using Haskell). I came across a Haskell based assignment which requires defining map and filter in terms of foldr. For the life of me I'm not fully understanding how to go about this.

For example when I define a map function like:

    map'            :: (a -> b) -> [a] -> [b]
    map' f []       = []
    map' f (x:xs)   = foldr (\x xs -> (f x):xs) [] xs
    

I don't know why the first element of the list is always ignored. Meaning that:

    map' (*2) [1,2,3,4]
    

results in \[4,6,8\] instead of \[2,4,6,8\]

Similarly, my filter' function:

    filter'             :: (a -> Bool) -> [a] -> [a]
    filter' p []        = []
    filter' p (x:xs)    = foldr (\x xs -> if p x then x:xs else xs ) [] xs
    

when run as:

    filter' even [2,3,4,5,6]
    

results in \[4,6\] instead of \[2,4,6\]

Why would this be the case? And how SHOULD I have defined these functions to get the expected results? I'm assuming something is wrong with my lambda expressions...

## Answer
        
I wish I could just comment, but alas, I don't have enough karma.

The other answers are all good ones, but I think the biggest confusion seems to be stemming from your use of x and xs.

If you rewrote it as

    map'            :: (a -> b) -> [a] -> [b]
    map' f []       = []
    map' f (x:xs)   = foldr (\y ys -> (f y):ys) [] xs
    

you would clearly see that `x` is not even mentioned on the right-hand side, so there's no way that it could be in the solution.

Cheers
