
# Zip with default value instead of dropping values?

## Question
        
I'm looking for a function in haskell to zip two lists that may vary in length.  
All zip functions I could find just drop all values of a lists that is longer than the other.  

For example: In my exercise I have two example lists.  
If the first one is shorter than the second one I have to fill up using 0's. Otherwise I have to use 1's.  
I'm not allowed to use any recursion. I just have to use higher order functions.

Is there any function I can use?  
I really could not find any solution so far.

## Answer
        
You can append an inifinte list of 0 or 1 to each list and then take the number you need from the result zipped list:

    zipWithDefault :: a -> b -> [a] -> [b] -> [(a,b)]
    zipWithDefault da db la lb = let len = max (length la) (length lb)
                                     la' = la ++ (repeat da)
                                     lb' = lb ++ (repeat db)
                                 in take len $ zip la' lb'
