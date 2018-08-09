
# How to zip multiple lists in Haskell?

## Question
        
In python `zip` function accepts arbitrary number of lists and zips them together.

    >>> l1 = [1,2,3]
    >>> l2 = [5,6,7]
    >>> l3 = [7,4,8]
    >>> zip(l1,l2,l3)
    [(1, 5, 7), (2, 6, 4), (3, 7, 8)]
    >>> 
    

How can I `zip` together multiple lists in haskell?

## Answer
        
A generalization of zip can be achieved using [Applicative Notation](http://hackage.haskell.org/packages/archive/base/4.6.0.0/doc/html/Control-Applicative.html). It's a bit unpleasant to use because of the newtype wrapping/unwrapping, but if you are doing something that can't be done with a `zipWithn` for reasonably small n, you are probably already at a high enough level of abstraction where the notational pains are absent anyway.

The type is `ZipList a`, and its applicative instance zips together lists. For example:

    (+) <$> ZipList [1,2] <*> ZipList [3,4] == ZipList [4,6]
    

This generalizes to functions of arbitrary arity and type using partial application:

    (+) <$> ZipList [1,2]  :: ZipList (Int -> Int)
    

See how (+) is partially applied here?

If you don't like adding ZipList and getZipList everywhere, you could recreate the notation easily enough:

    (<$>) :: (a -> b) -> [a] -> [b]
    (<$>) = map
    
    (<*>) :: [a -> b] -> [a] -> [b]
    (<*>) = zipWith ($)
    

Then the notation for `zipWith f a b c d ...` is:

    f <$> a <*> b <*> c <*> d <*> ...
    

Applicative notation is a very powerful and general technique that has much wider scope than just generalized zipping. See the [Typeclassopedia](http://www.haskell.org/haskellwiki/Typeclassopedia) for more on Applicative notation.
