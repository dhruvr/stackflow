
# What is fusion in Haskell?

## Question
        
Every now and again I have been noticing the following in Haskell documentation: (for example in [`Data.Text`](https://hackage.haskell.org/package/text-1.2.2.1/docs/Data-Text.html)):

> Subject to fusion

What is _fusion_ and how do I use it?

## Answer
        
In general, fusion refers to transformations whose purpose is to get rid of intermediate data structures. You _fuse_ function calls that result in wasteful memory allocations into something more efficient. This is actually IMO one of the biggest applications of Haskell being pure. And you pretty much don't need to do anything to get it, it comes for free through the GHC compiler.

### Haskell is pure

Because Haskell is pure, we get this thing called [referential transparency](https://wiki.haskell.org/Referential_transparency), which (from the link), means that "expression always evaluates to the same result in any context"1. That means that I can do very general program level manipulations without changing what the program will actually output. For example, even without knowing what `x`, `y`, `z` and `w` are I always know that

     ((x ++ y) ++ z) ++ w
    

will evaluate to the same thing as

     x ++ (y ++ (z ++ w))
    

yet the second one will in practice involve less memory allocations (since `x ++ y` requires reallocating whole prefix of the output list).

### Rewrite rules

In fact, there are a whole lot of this sort of optimization we can do, and, because Haskell is pure, we can basically just move whole expressions around (replacing `x`, `y`, `z`, or `w` for actual lists or expressions that evaluate to lists in the example above changes nothing). This becomes a pretty mechanical process.

Furthermore, it turns out that you can come up with a lot of equivalences for higher order functions ([Theorems for free!](https://www.mpi-sws.org/~dreyer/tor/papers/wadler.pdf)). For example,

    map f (map g xs) = map (f . g) xs
    

no matter what `f`, `g`, and `xs` are (the two sides are semantically equal). Yet while the two sides of this equation produce the same value output, the left hand side is always worse in efficiency: it ends up allocating space for an intermediate list `map g xs`, that is immediately thrown away. We'd like to tell the compiler to, whenever it encounters something like `map f (map g xs)`, replace it with `map (f . g) xs`. And, for GHC, that is through [rewrite rules](https://downloads.haskell.org/~ghc/7.0.1/docs/html/users_guide/rewrite-rules.html):

    {-# RULES     "map/map"    forall f g xs.  map f (map g xs) = map (f.g) xs #-}
    

The `f`, `g`, and `xs` can be matched against any expressions, not just variables (so something like `map (+1) (map (*2) ([1,2] ++ [3,4]))` gets transformed into `map ((+1) . (*2)) ([1,2] ++ [3,4])`. ([There doesn't appear to be a good way to search for rewrite rules](https://stackoverflow.com/questions/38651602/searching-for-rewrite-rules), so I compiled a [list](https://gist.github.com/harpocrates/e95ce275a2220dfbd50b102e1e533556)). [This paper](http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.89.3092&rep=rep1&type=pdf) explains the motivation and workings of GHC rewrite rules.

### So that's how GHC optimizes `map`?

Actually, not quite. The thing above is [short-cut fusion](https://wiki.haskell.org/Short_cut_fusion). The name sort of implies the drawback: it doesn't scale too well and is annoying to debug. You end up having to write a ton of ad-hoc rules for all arrangements of the same common functions. Then, you hope that repeated application of rewrite rules will simplify your expressions nicely.

It turns out that we can do even better in some cases by organizing our re-write rules so that we build up some intermediate normal form and then have rules targeting that intermediate form. This way, we start getting "hot" paths of rewrite rules.

Probably the most advanced of these systems is [stream fusion](http://citeseer.ist.psu.edu/viewdoc/download?doi=10.1.1.104.7401&rep=rep1&type=pdf) targeting coinductive sequences (basically lazy sequences like lists). Check out [this thesis](http://community.haskell.org/~duncan/thesis.pdf) and [this paper](http://research.microsoft.com/en-us/um/people/simonpj/papers/ndp/haskell-beats-C.pdf) (which is actually pretty much how the [`vector`](http://hackage.haskell.org/package/vector) package is implemented). For example, in `vector`, your code gets first transformed into an intermediate form involving `Stream`s and `Bundle`s, is optimized in that form, then gets transformed back into vectors.

### And... `Data.Text`?

`Data.Text` uses stream fusion to minimize the number of memory allocations that occur (I think this is especially important for the strict variant). If you check out the [source](https://hackage.haskell.org/package/text-1.2.2.1/docs/src/Data-Text.html#pack), you'll see that the functions "subject to fusion" actually manipulate [`Stream`s](https://hackage.haskell.org/package/text-1.2.2.1/docs/Data-Text-Internal-Fusion.html) for the most part (they are of the general form `unstream . (stuff manipulating stream) . stream`) and there are a bunch of `RULES` pragmas for transforming `Stream`s. In the end, any combination of these functions is supposed to get fused so that only one allocation needs to occur.

### So, what do I need to take away for my everyday coding?

The only real way to know when your code is subject to fusion is to have a good understanding of the rewrite rules involved and understand well how GHC works. That said, there is one thing that you _should_ do: try to use non-recursive higher order functions when possible, since these can be (at least for now, but in general will always be more) easily fused.

### Complications

Because fusion in Haskell occurs through repeated application of rewrite rules, it suffices to convince yourself of each rewrite rule's correctness to know that the whole "fused" program does the same thing as your original program does. Except there are edge cases relating to programs terminating. For example, one might think that

     reverse (reverse xs) = xs
    

yet that is clearly not true, since `head $ reverse (reverse [1..])` will not terminate yet `head [1..]` will. [More information from the Haskell Wiki](https://wiki.haskell.org/Correctness_of_short_cut_fusion).

* * *

1 This is actually true only provided that in these contexts the expression maintains the same type.
