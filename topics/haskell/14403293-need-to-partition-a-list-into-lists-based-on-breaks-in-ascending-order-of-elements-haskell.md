
# Need to partition a list into lists based on breaks in ascending order of elements (Haskell)

## Question
        
Say I have any list like this:

    [4,5,6,7,1,2,3,4,5,6,1,2]
    

I need a Haskell function that will transform this list into a list of lists which are composed of the segments of the original list which form a series in ascending order. So the result should look like this:

    [[4,5,6,7],[1,2,3,4,5,6],[1,2]]
    

Any suggestions?

## Answer
        
    ascend :: Ord a => [a] -> [[a]]
    ascend xs = foldr f [] xs
      where
        f a []  = [[a]]
        f a xs'@(y:ys) | a < head y = (a:y):ys
                       | otherwise = [a]:xs'
    

In ghci

    *Main> ascend [4,5,6,7,1,2,3,4,5,6,1,2]
    [[4,5,6,7],[1,2,3,4,5,6],[1,2]]
