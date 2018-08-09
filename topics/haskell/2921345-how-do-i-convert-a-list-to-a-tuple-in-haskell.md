
# How do I convert a list to a tuple in Haskell?

## Question
        
How can I best convert a list to a tuple in Haskell:

    [1,2,3,4,5,6] -> (1,2,3,4,5,6)

## Answer
        
In a general way, you can't. Each size of tuple is a distinct type, whereas lists of any length are a single type. Thus, there's no good way to write a function that takes a list and returns a tuple of the same length--it wouldn't have a well-defined return type.

For instance, you could have functions like:

    tuplify2 :: [a] -> (a,a)
    tuplify2 [x,y] = (x,y)
    
    tuplify3 :: [a] -> (a,a,a)
    tuplify3 [x,y,z] = (x,y,z)
    

...but not one that does the job of both.

You _can_ write a generic version using various kinds of meta-programming, but you'd rarely want to.

Note that the same problem applies to other things, such as writing class instances for various tuples--take a look at [the source code for Data.Tuple](http://www.haskell.org/ghc/docs/6.12.2/html/libraries/base-4.2.0.1/src/Data-Tuple.html) from the standard libraries!
