
# Why is GHC complaining about non-exhaustive patterns?

## Question
        
When I compile the following code with GHC (using the `-Wall` flag):

    module Main where
    
    data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show)
    
    insert :: (Ord a) => a -> Tree a -> Tree a
    insert x EmptyTree = Node x EmptyTree EmptyTree
    insert x (Node a left right)
        | x == a = Node a left right
        | x < a = Node a (insert x left) right
        | x > a = Node a left (insert x right)
    
    main :: IO()
    main = do
        let nums = [1..10]::[Int]
        print . foldr insert EmptyTree $ nums
    

GHC complains that pattern matching in `insert` is non-exhaustive:

    test.hs|6| 1:
    ||     Warning: Pattern match(es) are non-exhaustive
    ||              In an equation for `insert': Patterns not matched: _ (Node _ _ _)
    

Why is GHC issuing this warning? It is pretty obvious that the pattern GHC complains about is handled in `insert x (Node a left right)`.

## Answer
        
Riccardo is correct, GHC doesn't infer that your guards can't possibly all be false. So accept his answer please.

I'm going to digress and talk about coding style.

Your motivation for not using `otherwise` may have been that it looks unsightly:

    insert :: (Ord a) => a -> Tree a -> Tree a
    insert x EmptyTree = Node x EmptyTree EmptyTree
    insert x (Node a left right)
        | x == a    = Node a left right
        | x < a     = Node a (insert x left) right
        | otherwise = Node a left (insert x right)
    

Looking at this code, a human reader must confirm to themselves that the final guard accepts precisely those cases where `x > a`.

We could instead write it like this:

    insert :: (Ord a) => a -> Tree a -> Tree a
    insert x EmptyTree = Node x EmptyTree EmptyTree
    insert x (Node a left right) = case x `compare` a of
        EQ -> Node a left right
        LT -> Node a (insert x left) right
        GT -> Node a left (insert x right)
    

The `Ordering` type returned by `compare` has only the three values `EQ`, `LT`, and `GT`, so GHC can confirm that you've covered all possibilities, and a human reader can easily see that you've covered them correctly.

This is also more efficient code: we call `compare` once, instead of calling `==` and then probably calling `<` as well.

Now I'm going to digress some more and talk about laziness.

You've probably also written a function similar to this:

    contains :: (Ord a) => a -> Tree a -> Bool
    contains _ EmptyTree = False
    contains x (Node a left right) = case x `compare` a of
        EQ -> True
        ...
    

When `x == a`, you need to know that the tree uses the `Node` constructor, and that its first argument is equal to `x`. You don't need to know what either of the subtrees are.

But now look back at my definition of `insert` above. When the tree it's given is a `Node`, it always returns a `Node` whose first argument is always `a`. But it doesn't state that up front: instead it evaluates ``x `compare` a``.

We can rewrite `insert` to perform the comparison as late as possible:

    insert :: (Ord a) => a -> Tree a -> Tree a
    insert x EmptyTree = Node x EmptyTree EmptyTree
    insert x (Node a left right) = Node a newLeft newRight
      where comparison = x `compare` a
            newLeft  = if comparison == LT then insert x left  else left
            newRight = if comparison == GT then insert x right else right
    

Now we return the `Node a` bit as soon as possible --- even if the comparison throws an error! --- and we still perform the comparison once at most.
