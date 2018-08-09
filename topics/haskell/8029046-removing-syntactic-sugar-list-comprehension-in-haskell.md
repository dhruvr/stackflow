
# Removing syntactic sugar: List comprehension in Haskell

## Question
        
Can I unsugar list comprehension in this expression:

    [(i,j) | i <- [1..4], j <- [i+1..4]]
    

This is the output:

    [(1,2),(1,3),(1,4),(2,3),(2,4),(3,4)]
    

How can I, with map, filter and so on, write that piece of code?

**edit**

Here an other:

    [(i,j,k) | i <- [1..6], j <- [i+1..6],k <- [j+1..6]]
    

This is the output:

    [(1,2,3),(1,2,4),(1,2,5),(1,2,6),(1,3,4),(1,3,5),(1,3,6),(1,4,5),(1,4,6),(1,5,6),(2,3,4),(2,3,5),(2,3,6),(2,4,5),(2,4,6),(2,5,6),(3,4,5),(3,4,6),(3,5,6),(4,5,6)]

## Answer
        
List comprehensions (in fact, Monad comprehensions) can be desugared into `do` notation.

    do i <- [1..4]
       j <- [i+1..4]
       return (i,j)
    

Which can be desugared as usual:

    [1..4]   >>= \i ->
    [i+1..4] >>= \j ->
    return (i,j)
    

It is well known that `a >>= \x -> return b` is the same as `fmap (\x -> b) a`. So an intermediate desugaring step:

    [1..4] >>= \i -> 
    fmap (\j -> (i,j)) [i+1..4]
    

For lists, `(>>=) = flip concatMap`, and `fmap = map`

    (flip concatMap) [1..4] (\i -> map (\j -> (i,j) [i+1..4])
    

`flip` simply switches the order of the inputs.

    concatMap (\i -> map (\j -> (i,j)) [i+1..4]) [1..4]
    

And this is how you wind up with Tsuyoshi's answer.

* * *

The second can similarly be desugared into:

    concatMap (\i ->
      concatMap (\j ->
        map       (\k ->
          (i,j,k))
        [j+1..6])
      [i+1..6])
    [1..6]
