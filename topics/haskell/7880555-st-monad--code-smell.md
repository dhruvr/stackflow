
# ST Monad == code smell?

## Question
        
I'm working on implementing the [UCT](http://citeseer.ist.psu.edu/viewdoc/summary?doi=10.1.1.102.1296) algorithm in Haskell, which requires a fair amount of data juggling. Without getting into too much detail, it's a simulation algorithm where, at each "step," a leaf node in the search tree is selected based on some statistical properties, a new child node is constructed at that leaf, and the stats corresponding to the new leaf and all of its ancestors are updated.

Given all that juggling, I'm not really sharp enough to figure out how to make the whole search tree a nice immutable data structure à la [Okasaki](http://www.cs.cmu.edu/~rwh/theses/okasaki.pdf). Instead, I've been playing around with the `ST` monad a bit, creating structures composed of mutable `STRef`s. A contrived example (unrelated to UCT):

    import Control.Monad
    import Control.Monad.ST
    import Data.STRef
    
    data STRefPair s a b = STRefPair { left :: STRef s a, right :: STRef s b }
    
    mkStRefPair :: a -> b -> ST s (STRefPair s a b)
    mkStRefPair a b = do
        a' <- newSTRef a
        b' <- newSTRef b
        return $ STRefPair a' b'
    
    derp :: (Num a, Num b) => STRefPair s a b -> ST s ()
    derp p = do
        modifySTRef (left p) (\x -> x + 1)
        modifySTRef (right p) (\x -> x - 1)
    
    herp :: (Num a, Num b) => (a, b)
    herp = runST $ do
        p <- mkStRefPair 0 0
        replicateM_ 10 $ derp p
        a <- readSTRef $ left p
        b <- readSTRef $ right p
        return (a, b)
    
    main = print herp -- should print (10, -10)
    

Obviously this particular example would be much easier to write without using `ST`, but hopefully it's clear where I'm going with this... if I were to apply this sort of style to my UCT use case, is that wrong-headed?

Somebody asked a [similar question](https://stackoverflow.com/questions/607830/use-of-haskell-state-monad-a-code-smell) here a couple years back, but I think my question is a bit different... I have no problem using monads to encapsulate mutable state when appropriate, but it's that "when appropriate" clause that gets me. I'm worried that I'm reverting to an object-oriented mindset prematurely, where I have a bunch of objects with getters and setters. Not exactly idiomatic Haskell...

On the other hand, if it _is_ a reasonable coding style for some set of problems, I guess my question becomes: are there any well-known ways to keep this kind of code readable and maintainable? I'm sort of grossed out by all the explicit reads and writes, and especially grossed out by having to translate from my `STRef`-based structures inside the `ST` monad to isomorphic but immutable structures outside.

## Answer
        
I don't use ST much, but sometimes it is just the best solution. This can be in many scenarios:

*   There are already well-known, efficient ways to solve a problem. Quicksort is a perfect example of this. It is known for its speed and in-place behavior, which cannot be imitated by pure code very well.
*   You need rigid time and space bounds. Especially with lazy evaluation (and Haskell doesn't even specify whether there is lazy evaluation, just that it is non-strict), the behavior of your programs can be very unpredictable. Whether there is a memory leak could depend on whether a certain optimization is enabled. This is very different from imperative code, which has a fixed set of variables (usually) and defined evaluation order.
*   You've got a deadline. Although the pure style is almost always better practice and cleaner code, if you are used to writing imperatively and need the code soon, starting imperative and moving to functional later is a perfectly reasonable choice.

When I do use ST (and other monads), I try to follow these general guidelines:

*   Use [Applicative](http://www.haskell.org/ghc/docs/latest/html/libraries/base/Control-Applicative.html) style often. This makes the code easier to read and, if you do switch to an immutable version, much easier to convert. Not only that, but Applicative style is much more compact.
*   Don't just use ST. If you program only in ST, the result will be no better than a huge C program, possibly worse because of the explicit reads and writes. Instead, intersperse pure Haskell code where it applies. I often find myself using things like `STRef s (Map k [v])`. The map itself is being mutated, but much of the heavy lifting is done purely.
*   Don't remake libraries if you don't have to. A lot of code written for IO can be cleanly, and fairly mechanically, converted to ST. Replacing all the `IORef`s with `STRef`s and `IO`s with `ST`s in Data.HashTable was much easier than writing a hand-coded hash table implementation would have been, and probably faster too.

One last note - if you are having trouble with the explicit reads and writes, there are [ways around it](http://augustss.blogspot.com/2007/08/programming-in-c-ummm-haskell-heres.html).
