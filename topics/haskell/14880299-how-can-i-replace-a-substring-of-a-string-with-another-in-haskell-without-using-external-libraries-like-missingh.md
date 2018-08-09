
# How can I replace a substring of a string with another in Haskell without using external Libraries like MissingH?

## Question
        
I would like to replace a substring with a string in Haskell, without using external libraries, and, if it is possible, with good performance.

I thought about using the [**`Data.Text`**](https://hackage.haskell.org/package/text-1.2.1.3/docs/Data-Text.html) replace functions, but I don't want to port my entire program to use the `Text` type instead of `Strings`. Would packing the `String` into a `Text` value, then replacing what I wanted to, then unpacking that Text value to a `String` be slow on a lot of `Strings`?

## Answer
        
Try this (untested):

    replace :: Eq a => [a] -> [a] -> [a] -> [a]
    replace needle replacement haystack
      = case begins haystack needle of
          Just remains -> replacement ++ remains
          Nothing      -> case haystack of
                            []     -> []
                            x : xs -> x : replace needle replacement xs
    
    begins :: Eq a => [a] -> [a] -> Maybe [a]
    begins haystack []                = Just haystack
    begins (x : xs) (y : ys) | x == y = begins xs ys
    begins _        _                 = Nothing
    

But in general you will get performance gains from switching your program to use `Text`s instead of `String`s.
