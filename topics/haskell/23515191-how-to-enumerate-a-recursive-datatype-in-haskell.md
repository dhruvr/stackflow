
# How to enumerate a recursive datatype in Haskell?

## Question
        
[This blog post](http://lukepalmer.wordpress.com/2008/05/02/enumerating-a-context-free-language/) has an interesting explanation of how to use the Omega monad to enumerate an arbitrary grammar diagonally. He offers an example of how to do so, resulting in an infinite sequence of strings. I'd like to do the same, except that, instead of generating a list of strings, it generates a list of an actual datatype. For example,

     data T = A | B T | C T T
    

Would generate

    A, B A, C A A, C (B A) A... 
    

Or something similar. Unfortunately my Haskell skills are still maturing and after some hours playing it I couldn't manage to do what I want. How can that be done?

As requested, one of my attempts (I have tried too many things...):

    import Control.Monad.Omega
    
    data T = A | B T | C T T deriving (Show)
    
    a = [A] 
            ++ (do { x <- each a; return (B x) })
            ++ (do { x <- each a; y <- each a; return (C x y) })
    
    main = print $ take 10 $ a

## Answer
        
My first ugly approach was:

    allTerms :: Omega T
    allTerms = do
      which <- each [ 1,2,3 ]
      if which == 1 then
        return A
      else if which == 2 then do
        x <- allTerms
        return $ B x
      else do
        x <- allTerms
        y <- allTerms
        return $ C x y
    

But then, after some cleaning up I reached this one liner

    import Control.Applicative
    import Control.Monad.Omega
    import Control.Monad
    
    allTerms :: Omega T
    allTerms = join $ each [return A, B <$> allTerms, C <$> allTerms <*> allTerms]
    

Note that order matters: `return A` has to be the first choice in the list above, or `allTerms` will not terminate. Basically, the `Omega` monad ensures a "fair scheduling" among choices, saving you from e.g. `infiniteList ++ something`, but does not prevent infinite recursion.

* * *

An even more elegant solution was suggested by [Crazy FIZRUK](https://stackoverflow.com/users/442535/crazy-fizruk), exploiting the `Alternative` instance of `Omega`.

    import Control.Applicative
    import Data.Foldable (asum)
    import Control.Monad.Omega
    
    allTerms :: Omega T
    allTerms = asum [ pure A
                    , B <$> allTerms
                    , C <$> allTerms <*> allTerms
                    ]
