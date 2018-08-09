
# What indentation is required for a case statement within a let statement?

## Question
        
Working in haskell, found odd behavior, stripped it down to bare bones

This Works

    a :: Bool
    a = case True of
        True -> True
        False -> False
    

But when I try

    b :: IO Bool
    b = do
        let b' = case True of
            True -> True
            False -> False
        return b'
    

I get

    ghci>:l test.hs
    [1 of 1] Compiling Main             ( test.hs, interpreted )
    
    test.hs:16:14: parse error on input ‘->’
    Failed, modules loaded: none.
    

So I try

    c :: IO Bool
    c = do
        let c' = case True of
                True -> True
                False -> False
        return c'
    

And this works.

What? Why? Why do I need an extra indent in this case? I can't find anything on this, probably because these keyword are so short and common in everyday language. Is there some spec that explains this behavior?

## Answer
        
I don't have the exact wording from the spec, but this [Wikibook page](https://en.wikibooks.org/wiki/Haskell/Indentation) explains the issue quite clearly.

The reason why it works like this is simple: to support binding multiple variables via a single let-group, such as:

    c = do
        let c' = …
            d  = …
            e  = …
        return c'
    

Your `True -> …` and `False -> …` are mistakenly interpreted as additional variables to be bound.
