
# Cartesian product of 2 lists in Haskell

## Question
      
I wish to produce the Cartesian product of 2 lists in Haskell, but I cannot work out how to do it. The cartesian product gives all combinations of the list elements:

    xs = [1,2,3]
    ys = [4,5,6]
    
    cartProd :: [a] -> [b] -> [(a,b)]
    cartProd xs ys ==> [(1,4),(1,5),(1,6),(2,4),(2,5),(2,6),(3,4),(3,5),(3,6)]
    

This is not an actual homework question and is not related to any such question, but the way in which this problem is solved may help with one I am stuck on.
## Answer
      
This is very easy with list comprehensions. To get the cartesian product of the lists `xs` and `ys`, we just need to take the tuple `(x,y)` for each element `x` in `xs` and each element `y` in `ys`.

This gives us the following list comprehension:

    cartProd xs ys = [(x,y) | x <- xs, y <- ys]
    