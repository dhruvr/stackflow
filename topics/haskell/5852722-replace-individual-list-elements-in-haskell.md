
# Replace individual list elements in Haskell?

## Question
        
I have a list of elements and I wish to update them:

from this: `["Off","Off","Off","Off"]`

to this: `["Off","Off","On","Off"]`

As I am somewhat new to Haskell, I have been using `(x:xs)!!y` to extract and update individual components using the function:

    replace y z [] = []
    replace y z (x:xs)
      | x==y           = z:replace y z xs
      | otherwise      = x:replace y z xs
    

and then entering the following in ghci: `(replace "Off" "On" ["Off",'Off","Off","Off"]) !! 2`

I get the following: `"On"`

I seem to be able to extract and convert elements of a list but I can't seem to get a list up with the single element converted.

Any help regarding this matter would be appreciated.

## Answer
        
I'm not sure what you are trying to do. If you only need to generate \["Off","Off","On","Off"\] you can do it explicitly. Generally speaking, one should avoid modifying state in haskell.

Perhaps what you want is a function to "modify" (generate a new element with a different value) the nth element of a list? Don gives a very general approach to this kind of problem. You can also use explicit recursion:

     replaceNth :: Int -> a -> [a] -> [a]
     replaceNth _ _ [] = []
     replaceNth n newVal (x:xs)
       | n == 0 = newVal:xs
       | otherwise = x:replaceNth (n-1) newVal xs
    

Haskell provides excellent features for list manipulation. If you dont know them already `filter`, `map`, and `foldr`/`foldl` are all worth looking at, as are list comprehensions.
