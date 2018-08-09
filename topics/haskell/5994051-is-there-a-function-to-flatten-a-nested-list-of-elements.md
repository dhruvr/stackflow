
# Is there a function to flatten a nested list of elements?

## Question
        
How can I flatten a nested list like this:

    [1, 2, 3, 4] == flatten [[[1,2],[3]],[[4]]]

## Answer
        
Since nobody else has given this, it is possible to define a function which will flatten lists of an arbitrary depth by using MultiParamTypeClasses. I haven't actually found it useful, but hopefully it could be considered an interesting hack. I got the idea from Oleg's polyvariadic function implementation.

    {-# LANGUAGE MultiParamTypeClasses, OverlappingInstances, FlexibleInstances #-}
    
    module Flatten where
    
    class Flatten i o where
      flatten :: [i] -> [o]
    
    instance Flatten a a where
      flatten = id
    
    instance Flatten i o => Flatten [i] o where 
      flatten = concatMap flatten
    

Now if you load it and run in ghci:

    *Flatten> let g = [1..5]
    *Flatten> flatten g :: [Integer]
    [1,2,3,4,5]
    *Flatten> let h = [[1,2,3],[4,5]]
    *Flatten> flatten h :: [Integer]
    [1,2,3,4,5]
    *Flatten> let i = [[[1,2],[3]],[],[[4,5],[6]]]
    *Flatten> :t i
    i :: [[[Integer]]]
    *Flatten> flatten i :: [Integer]
    [1,2,3,4,5,6]
    

Note that it's usually necessary to provide the result type annotation, because otherwise ghc can't figure out where to stop recursively applying the `flatten` class method. If you use a function with a monomorphic type that's sufficient however.

    *Flatten> :t sum
    sum :: Num a => [a] -> a
    *Flatten> sum $ flatten g
    
    <interactive>:1:7:
        No instance for (Flatten Integer a0)
          arising from a use of `flatten'
        Possible fix: add an instance declaration for (Flatten Integer a0)
        In the second argument of `($)', namely `flatten g'
        In the expression: sum $ flatten g
        In an equation for `it': it = sum $ flatten g
    *Flatten> let sumInt = sum :: [Integer] -> Integer
    *Flatten> sumInt $ flatten g
    15
    *Flatten> sumInt $ flatten h
    15
