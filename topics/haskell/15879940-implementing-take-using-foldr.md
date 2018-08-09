
# Implementing take using foldr

## Question
        
This is my `take` version using `foldr`:

    myTake n list = foldr step [] list
                    where step x y | (length y) < n = x : y
                                   | otherwise = y
    
    main = do print $ myTake 2 [1,2,3,4]
    

The output is not what I expect:

    [3,4]
    

I then tried to debug by inserting the length of `y` into itself and the result was:

    [3,2,1,0]
    

I don't understand why the lengths are inserted in decreasing order. Perhaps something obvious I missed?

## Answer
        
If you want to implement `take` using `foldr` you need to simulate traversing the list from left to right. The point is to make the folding function depend on an extra argument which encodes the logic you want and not only depend on the folded tail of the list.

    take :: Int -> [a] -> [a]
    take n xs = foldr step (const []) xs n
      where
        step x g 0 = []
        step x g n = x:g (n-1)
    

Here, `foldr` returns a function which takes a numeric argument and traverses the list from left to right taking from it the amount required. This will also work on infinite lists due to laziness. As soon as the extra argument reaches zero, `foldr` will short-circuit and return an empty list.
