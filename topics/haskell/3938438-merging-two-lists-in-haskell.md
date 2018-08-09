
# Merging two lists in Haskell

## Question
        
Can't figure out how to merge two lists **in the following way** in Haskell:

    INPUT:  [1,2,3,4,5] [11,12,13,14]
    
    OUTPUT: [1,11,2,12,3,13,4,14,5]

## Answer
        
    merge :: [a] -> [a] -> [a]
    merge xs     []     = xs
    merge []     ys     = ys
    merge (x:xs) (y:ys) = x : y : merge xs ys
