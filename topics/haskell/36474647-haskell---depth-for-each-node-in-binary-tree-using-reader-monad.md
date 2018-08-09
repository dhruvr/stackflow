
# Haskell - depth for each node in binary tree using Reader monad

## Question
        
I wrote the following code. It is working and using the `Reader` monad.

Could you give me some hints about code style in Haskell ? Mainly, I mean monads -- I am newbie.

    import Control.Monad.Reader
    
    data Tree a = Node a (Tree a) (Tree a)
                | Empty
    
    renumberM :: Tree a -> Reader Int (Tree Int)
    renumberM (Node _ l r) = ask >>= (\x -> 
                             return (Node x (runReader (local (+1) (renumberM l)) x) 
                                            (runReader (local (+1) (renumberM r)) x)))
    renumberM Empty = return Empty
    
    renumber'' :: Tree a -> Tree Int
    renumber'' t = runReader (renumberM t) 0

## Answer
        
I want to show you that your idea is an instance of a more general concept - _zipping_. Here's a version of your program that employs a simpler and more functional style.

Applicative Functors
--------------------

Here's the definition of [`Applicative`](http://hackage.haskell.org/package/base-4.8.2.0/docs/Control-Applicative.html#t:Applicative):

    class Functor f => Applicative f where
        pure :: a -> f a
        (<*>) :: f (a -> b) -> f a -> f b
    

You could say that a type `f x` is a _structure_ `f` containing some _values_ `x`. The function `<*>` takes a structure of functions (`f (a -> b)`) and applies it to a structure of arguments (`f a`) to produce a structure of results (`f b`).

Zippy Applicatives
------------------

One way to make `Tree` an applicative functor is by making `<*>` traverse the two trees in lock-step, _zipping_ them together just like [`zip`](http://hackage.haskell.org/package/base-4.8.2.0/docs/Prelude.html#v:zip) does with lists. Every time you encounter a `Node` in the tree of functions and a `Node` in the tree of arguments, you can pull the function out and apply it to the argument. You have to stop traversing when you reach the bottom of either of the trees.

    instance Applicative Tree where
        pure x = let t = Node x t t in t
        Empty <*> _ = Empty
        _ <*> Empty = Empty
        (Node f lf rf) <*> (Node x lx rx) = Node (f x) (lf <*> lx) (rf <*> rx)
    
    instance Functor Tree where
        fmap f x = pure f <*> x  -- as usual
    

`pure x` generates an infinite tree of `x`s. This works fine because Haskell is a lazy language.

         +-----x-----+
         |           |
      +--x--+     +--x--+
      |     |     |     |
    +-x-+ +-x-+ +-x-+ +-x-+
    |   | |   | |   | |   |
              etc
    

So the shape of the tree `t <*> pure x` is the same as the shape of `t`: you only stop traversing when you encounter an `Empty`, and there aren't any in `pure x`. (The same applies to `pure x <*> t`.)

This is a common way to make a data structure an instance of `Applicative`. For example, the standard library includes [`ZipList`](http://hackage.haskell.org/package/base-4.8.2.0/docs/Control-Applicative.html#t:ZipList), whose [`Applicative` instance](http://hackage.haskell.org/package/base-4.8.2.0/docs/src/Control.Applicative.html#ZipList) is very similar to that of our tree:

    newtype ZipList a = ZipList { getZipList :: [a] }
    instance Applicative ZipList where
        pure x = ZipList (repeat x)
        ZipList fs <*> ZipList xs = ZipList (zipWith ($) fs xs)
    

Once again, `pure` generates an infinite `ZipList`, and `<*>` consumes its arguments in lock-step.

The prototypical zippy Applicative, if you like, is the "reader" Applicative `(->) r`, which combines functions by applying them all to a fixed argument and collecting the results. So all [`Representable`](https://hackage.haskell.org/package/representable-functors-3.2.0.2/docs/Data-Functor-Representable.html) functors admit (at least) a zippy `Applicative` instance.

Using [some `Applicative` machinery](http://hackage.haskell.org/package/base-4.8.2.0/docs/Control-Applicative.html#v:liftA2), we can generalise the Prelude's `zip` to any applicative functor (although it'll only behave precisely like `zip` when the `Applicative` is zippy in nature - for example, with the regular `Applicative` instance for `[]` `zipA` will give you the Cartesian product of its arguments).

    zipA :: Applicative f => f a -> f b -> f (a, b)
    zipA = liftA2 (,)
    

Labelling as Zipping
--------------------

The plan is to zip the input tree together with an infinite tree containing the depth of each level. The output will be a tree with the same shape as the input tree (because the depths-tree is infinite), but each node will be labelled with its depth.

    depths :: Tree Integer
    depths = go 0
        where go n = let t = go (n+1) in Node n t t
    

This is what `depths` looks like:

         +-----0-----+
         |           |
      +--1--+     +--1--+
      |     |     |     |
    +-2-+ +-2-+ +-2-+ +-2-+
    |   | |   | |   | |   |
              etc
    

Now that we've set up the structures we need, labelling a tree is easy.

    labelDepths :: Tree a -> Tree (Integer, a)
    labelDepths = zipA depths
    

Relabelling a tree by throwing away the original labels, as you originally specified, [is easy too](http://hackage.haskell.org/package/base-4.8.2.0/docs/Control-Applicative.html#v:-42--62-).

    relabelDepths :: Tree a -> Tree Integer
    relabelDepths t = t *> depths
    

A quick test:

    ghci> let myT = Node 'x' (Node 'y' (Node 'z' Empty Empty) (Node 'a' Empty Empty)) (Node 'b' Empty Empty)
    ghci> labelDepths myT
    Node (0,'x') (Node (1,'y') (Node (2,'z') Empty Empty) (Node (2,'a') Empty Empty)) (Node (1,'b') Empty Empty)
    
        +--'x'-+                      +--(0,'x')-+
        |      |    labelDepths       |          |
     +-'y'-+  'b'       ~~>      +-(1,'y')-+  (1,'b')
     |     |                     |         |
    'z'   'a'                 (2,'z')   (2,'a')
    

You can devise different labelling schemes by varying the tree you zip along. Here's one which tells you the path you took to reach a node:

    data Step = L | R
    type Path = [Step]
    paths :: Tree Path
    paths = go []
        where go path = Node path (go (path ++ [L])) (go (path ++ [R]))
    
             +--------[ ]--------+
             |                   |
        +---[L]---+         +---[R]---+
        |         |         |         |
    +-[L,L]-+ +-[L,R]-+ +-[R,L]-+ +-[R,R]-+
    |       | |       | |       | |       |
                      etc
    

(The inefficient nesting of calls to `++` above can be mitigated using [difference lists](https://wiki.haskell.org/Difference_list).)

    labelPath :: Tree a -> Tree (Path, a)
    labelPath = zipA paths
    

* * *

As you continue to learn Haskell, you'll get better at spotting when a program is an example of a deeper concept. Setting up general structures, like I did with the `Applicative` instance above, quickly pays dividends in code reuse.
