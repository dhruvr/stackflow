
# Uses for Haskell id function

## Question
        
Which are the uses for [id function](http://www.zvon.org/other/haskell/Outputprelude/id_f.html) in Haskell?

## Answer
        
It's useful as an argument to [higher order functions](http://www.haskell.org/haskellwiki/Higher_order_function#Definition) (functions which take functions as arguments), where you want some particular value left unchanged.

**Example 1**: Leave a value alone if it is in a Just, otherwise, return a default of 7.

    Prelude Data.Maybe> :t maybe
    maybe :: b -> (a -> b) -> Maybe a -> b
    
    Prelude Data.Maybe> maybe 7 id (Just 2)
    2
    

**Example 2**: building up a function via a fold:

    Prelude Data.Maybe> :t foldr (.) id [(+2), (*7)]
    :: (Num a) => a -> a
    
    Prelude Data.Maybe> let f = foldr (.) id [(+2), (*7)]
    
    Prelude Data.Maybe> f 7
    51
    

We built a new function `f` by folding a list of functions together with `(.)`, using `id` as the base case.

**Example 3**: the base case for functions as monoids (simplified).

    instance Monoid (a -> a) where
            mempty        = id
            f `mappend` g = (f . g)
    

Similar to our example with fold, functions can be treated as concatenable values, with `id` serving for the empty case, and `(.)` as append.

**Example 4**: a trivial hash function.

    Data.HashTable> h <- new (==) id :: IO (HashTable Data.Int.Int32 Int)
    
    Data.HashTable> insert h 7 2
    
    Data.HashTable> Data.HashTable.lookup h 7
    Just 2
    

Hashtables require a hashing function. But what if your key is already hashed? Then pass the id function, to fill in as your hashing method, with zero performance overhead.
