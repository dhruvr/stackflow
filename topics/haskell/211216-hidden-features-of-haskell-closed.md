
# Hidden features of Haskell [closed]

## Question
        
What are the lesser-known but useful features of the Haskell programming language. (I understand the language itself is lesser-known, but work with me. Even explanations of the simple things in Haskell, like defining the Fibonacci sequence with one line of code, will get upvoted by me.)

*   Try to limit answers to the Haskell core
*   One feature per answer
*   Give an example and short description of the feature, not just a link to documentation
*   Label the feature using bold title as the first line

## Answer
        
**My brain just exploded**

If you try to compile this code:

    {-# LANGUAGE ExistentialQuantification #-}
    data Foo = forall a. Foo a
    ignorefoo f = 1 where Foo a = f
    

You will get this error message:

$ ghc Foo.hs

Foo.hs:3:22:
    My brain just exploded.
    I can't handle pattern bindings for existentially-quantified constructors.
    Instead, use a case-expression, or do-notation, to unpack the constructor.
    In the binding group for
        Foo a
    In a pattern binding: Foo a = f
    In the definition of `ignorefoo':
        ignorefoo f = 1
                    where
                        Foo a = f
