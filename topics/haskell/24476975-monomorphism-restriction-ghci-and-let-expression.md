
# Monomorphism Restriction, ghci, and let expression

## Question
        
This expression is incorrect.

    f = show
    

However, in **ghci** this is legit

    let f = show
    

Moreover, its type is changed to

    () -> String
    

Is there any explanation of this phenomenon?

## Answer
        
The **ghci** prompt behaves as if the [`ExtendedDefaultRules`](http://www.haskell.org/ghc/docs/latest/html/users_guide/interactive-evaluation.html#extended-default-rules) extension is enabled.

In particular this means that:

> The unit type () is added to the start of the standard list of types which are tried when doing type defaulting.

So to get the same behaviour from a source file, either compile with `-XExtendedDefaultRules`, or add `{-# LANGUAGE ExtendedDefaultRules #-}` to the top of the file.
