
# How to split a string in Haskell?

## Question
        
Is there a standard way to split a string in Haskell?

`lines` and `words` work great from spliting on a space or newline, but surely there is a standard way to split on a comma? I couldn't fint it at Hoogle?

To be specific, I'm looking for something where `split "," "my,comma,separated,list"` returns `["my","comma","separated","list"]`

Thanks.

## Answer
        
There is a package for this called [split](https://hackage.haskell.org/package/split/docs/Data-List-Split.html).

    cabal install split
    

Use it like this:

    ghci> import Data.List.Split
    ghci> splitOn "," "my,comma,separated,list"
    ["my","comma","separated","list"]
    

It comes with a lot of other functions for splitting on matching delimiters or having several delimiters.
