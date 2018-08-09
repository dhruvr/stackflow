
# Why is there no Show instance for functions?

## Question
        
Just a quick conceptual question, I am currently trying to learn and understand Haskell better.

I know the Show function is used to convert values to strings, but why can't function types be used with show?

    Prelude> (\x -> x*3)
    
    <interactive>:7:1:
        No instance for (Show (a0 -> a0))
          arising from a use of `print'
        Possible fix: add an instance declaration for (Show (a0 -> a0))
        In a stmt of an interactive GHCi command: print it
    Prelude>

## Answer
        
It's not that they can't, but that there's not usually a good reason to.

But if you'd like, you definitely can:

    Prelude> :{
    Prelude| instance Show (a -> b) where
    Prelude|    show _ = "A function."
    Prelude| :}
    Prelude> print (\x -> x + 7)
    A function.
    Prelude> print (\a b c -> a + b + c)
    A function.
    

If you'd like to `show` the textual representation of the function, well - you can't do that. Unlike metaprogramming languages like Ruby, JS, etc, Haskell code very little knowledge of its own internals.
