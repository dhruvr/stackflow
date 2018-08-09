
# How to define a rotates function

## Question
        
How to define a rotates function that generates all rotations of the given list?

For example: rotates `[1,2,3,4] =[[1,2,3,4],[2,3,4,1],[3,4,1,2],[4,1,2,3]]`

I wrote a shift function that can rearrange the order

     shift ::[Int]->[Int]
    
     shift x=tail ++ take 1 x
    

but I don't how to generate these new arrays and append them together.

## Answer
        
The following

    shift :: [a] -> Int -> [a]
    shift l n = drop n l  ++ take n l
    
    allRotations :: [a] -> [[a]]
    allRotations l = [ shift l i | i <- [0 .. (length l) -1]]
    

yields

    > ghci
    Prelude> :l test.hs
    [1 of 1] Compiling Main             ( test.hs, interpreted )
    Ok, modules loaded: Main.
    *Main> allRotations [1,2,3,4]
    [[1,2,3,4],[2,3,4,1],[3,4,1,2],[4,1,2,3]]
    

which is as you expect.

I think this is fairly readable, although not particularly efficient (no memoisation of previous shifts occurs).

* * *

If you care about efficiency, then

    shift :: [a] -> [a]
    shift [] = []
    shift (x:xs) = xs ++ [x]
    
    allRotations :: [a] -> [[a]]
    allRotations l = take (length l) (iterate shift l)
    

will allow you to reuse the results of previous shifts, and avoid recomputing them.

Note that `iterate` returns an infinite list, and due to lazy evaluation, we only ever evaluate it up to `length l` into the list.

* * *

Note that in the first part, I've extended your shift function to ask how much to shift, and I've then a list comprehension for `allRotations`.
