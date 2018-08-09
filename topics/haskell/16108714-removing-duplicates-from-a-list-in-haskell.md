
# Removing duplicates from a list in Haskell

## Question
        
I'm trying to define a function which will remove duplicates from a list. So far I have a working implementation:

    rmdups :: Eq a => [a] -> [a]
    rmdups [] = []
    rmdups (x:xs)   | x `elem` xs   = rmdups xs
                    | otherwise     = x : rmdups xs
    

However I'd like to rework this without using `elem`. What would be the best method for this?

I'd like to do this using my own function and not `nub` or `nubBy`.

## Answer
        
I don't think you'll be able to do it without `elem` (or your own re-implementation of it).

However, there is a semantic issue with your implementation. When elements are duplicated you're keeping the _last_ one. Personally, I'd expect it to keep the first duplicate item and drop the rest.

    *Main> rmdups "abacd"
    "bacd"
    

The solution is to thread the 'seen' elements through as a state variable.

    removeDuplicates :: Eq a => [a] -> [a]
    removeDuplicates = rdHelper []
        where rdHelper seen [] = seen
              rdHelper seen (x:xs)
                  | x `elem` seen = rdHelper seen xs
                  | otherwise = rdHelper (seen ++ [x]) xs
    

This is more-or-less how `nub` is implemented in the standard library (read the source [here](http://hackage.haskell.org/package/base-4.7.0.0/docs/src/Data-List.html#nub)). The small difference in `nub`'s implementation ensures that it is [non-strict](http://www.haskell.org/haskellwiki/Non-strict_semantics), while `removeDuplicates` above is strict (it consumes the entire list before returning).

Primitive recursion is actually overkill here, if you're not worried about strictness. `removeDuplicates` can be implemented in one line with `foldl`:

    removeDuplicates2 = foldl (\seen x -> if x `elem` seen
                                          then seen
                                          else seen ++ [x]) []
