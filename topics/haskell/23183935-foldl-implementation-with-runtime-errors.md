
# foldl Implementation with Runtime Errors

## Question
        
[Learn You a Haskell](http://learnyouahaskell.com/higher-order-functions#folds) explains `foldl1`:

> The foldl1 and foldr1 functions work much like foldl and foldr, only you don't need to provide them with an explicit starting value. They assume the first (or last) element of the list to be the starting value and then start the fold with the element next to it. ...
> 
> Because they depend on the lists they fold up having at least one element, they cause runtime errors if called with empty lists

I figured its implementation is, more or less, the following:

    foldl1' :: (a -> a -> a) -> [a] -> a
    foldl1' f ys = foldl f (head ys) (tail ys)
    

But, this potential run-time error troubles me.

Why not implement `foldlOption` in the following way?

    foldlOption :: (a -> a -> a) -> [a] -> Maybe a
    foldlOption f [] = Nothing
    foldlOption f ys = Just (foldl f (head ys) (tail ys))
    

**REPL**

    *Main> foldlOption (\acc elem -> if (elem > acc) then elem else acc) []
    Nothing
    
    -- find max
    *Main> foldlOption (\acc elem -> if (elem > acc) then elem else acc) [1,100,2,3]
    Just 100
    

**EDITED**

Updated `foldl1`'s and `foldlOption`'s definitions to use `tail ys` as the last argument to `foldl`, not `ys` per [Lee Duhem](https://stackoverflow.com/users/1004301/lee-duhem)'s correction. .

## Answer
        
There's actually no good reason why not to do this. Many of the functions in Haskell's prelude like `head`, `tail`, `init`, and many many others fail unnecessarily.

It would be much nicer for them to explicitly note their failure in the types, but that's unfortunately just not what happened when Prelude was standardized and we can't very well change several core functions like `head`!

Nowadays I recommend simply not using many of these functions and opting for pattern matching, or Gabriel Gonzalez's [errors](http://hackage.haskell.org/package/errors) library which provides alternate versions of prelude's partial functions which fail properly.

For example in `Control.Error.Safe` there's

    foldl1Err :: e -> (a -> a -> a) -> [a] -> Either e a
    

and errors also exports safe, a similar library with `Maybe`'s which has the function

    foldl1May :: (a -> a -> a) -> [a] -> Maybe a
    

exactly like what you wanted :)
