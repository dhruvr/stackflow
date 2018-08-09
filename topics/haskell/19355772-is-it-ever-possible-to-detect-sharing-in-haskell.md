
# Is it ever possible to detect sharing in Haskell?

## Question
        
In Scheme, the primitive `eq?` tests whether its arguments are the same object. For example, in the following list

    (define lst
      (let (x (list 'a 'b))
        (cons x x)))
    

The result of

    (eq? (car x) (cdr x))
    

is true, and moreover it is true _without_ having to peer into `(car x)` and `(cdr x)`. This allows you to write efficient equality tests for data structures that have a lot of sharing.

Is the same thing ever possible in Haskell? For example, consider the following binary tree implementation

    data Tree a = Tip | Bin a (Tree a) (Tree a)
    
    left  (Bin _ l _) = l
    right (Bin _ _ r) = r
    
    mkTree n :: Int -> Tree Int
    mkTree 0 = Tip
    mkTree n = let t = mkTree (n-1) in Bin n t t
    

which has sharing at every level. If I create a tree with `let tree = mkTree 30` and I want to see if `left tree` and `right tree` are equal, naively I have to traverse over a billion nodes to discover that they are the same tree, which should be obvious because of data sharing.

I don't expect there is a simple way to discover data sharing in Haskell, but I wondered what the typical approaches to dealing with issues like this are, when it would be good to detect sharing for efficiency purposes (or e.g. to detect cyclic data structures).

Are there `unsafe` primitives that can detect sharing? Is there a well-known way to build data structures with explicit pointers, so that you can compare pointer equality?

## Answer
        
There's lots of approaches.

1.  Generate unique IDs and stick everything in a finite map (e.g. [`IntMap`](http://hackage.haskell.org/package/containers-0.5.3.1/docs/Data-IntMap.html)).
2.  The refined version of the last choice is to make an explicit graph, e.g. using [fgl](http://hackage.haskell.org/package/fgl).
3.  Use [stable names](https://stackoverflow.com/questions/5701893/python-is-like-equality-operator-for-haskell-ghc).
4.  Use [`IORef`s](http://hackage.haskell.org/package/base-4.6.0.1/docs/Data-IORef.html) ([see also](https://stackoverflow.com/questions/1717553/pointer-equality-in-haskell)), which have both `Eq` and `Ord` instances regardless of the contained type.
5.  There are libraries for [observable sharing](https://stackoverflow.com/questions/19337153/functions-of-possibly-cyclic-data-structures).
6.  As mentioned above, there is `reallyUnsafePtrEquality#` but you should understand what's really unsafe about it before you use it!

See also [this answer about avoiding equality checks altogether](https://stackoverflow.com/questions/1976247/efficiency-of-equality-in-haskell).
