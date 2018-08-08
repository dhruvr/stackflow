
# Memory footprint of Haskell data types

## Question
      
How can I find the actual amount of memory required to store a value of some data type in Haskell (mostly with GHC)? Is it possible to evaluate it at runtime (e.g. in GHCi) or is it possible to estimate memory requirements of a compound data type from its components?

In general, if memory requirements of types `a` and `b` are known, what is the memory overhead of algebraic data types such as:

    data Uno = Uno a
    data Due = Due a b
    

For example, how many bytes in memory do these values occupy?

    1 :: Int8
    1 :: Integer
    2^100 :: Integer
    \x -> x + 1
    (1 :: Int8, 2 :: Int8)
    [1] :: [Int8]
    Just (1 :: Int8)
    Nothing
    

I understand that actual memory allocation is higher due to delayed garbage collection. It may be significantly different due to lazy evaluation (and thunk size is not related to the size of the value). The question is, given a data type, how much memory does its value take when fully evaluated?

I found there is a `:set +s` option in GHCi to see memory stats, but it is not clear how to estimate the memory footprint of a single value.
## Answer
      
(The following applies to GHC, other compilers may use different storage conventions)

Rule of thumb: **a constructor costs one word for a header, and one word for each field**. Exception: a constructor with no fields (like `Nothing` or `True`) takes no space, because GHC creates a single instance of these constructors and shares it amongst all uses.

A word is 4 bytes on a 32-bit machine, and 8 bytes on a 64-bit machine.

So e.g.

    data Uno = Uno a
    data Due = Due a b
    

an `Uno` takes 2 words, and a `Due` takes 3.

The `Int` type is defined as

    data Int = I# Int#
    

now, `Int#` takes one word, so `Int` takes 2 in total. Most unboxed types take one word, the exceptions being `Int64#`, `Word64#`, and `Double#` (on a 32-bit machine) which take 2. GHC actually has a cache of small values of type `Int` and `Char`, so in many cases these take no heap space at all. A `String` only requires space for the list cells, unless you use `Char`s > 255.

An `Int8` has identical representation to `Int`. `Integer` is defined like this:

    data Integer
      = S# Int#                            -- small integers
      | J# Int# ByteArray#                 -- large integers
    

so a small `Integer` (`S#`) takes 2 words, but a large integer takes a variable amount of space depending on its value. A `ByteArray#` takes 2 words (header + size) plus space for the array itself.

Note that **a constructor defined with `newtype` is free**. `newtype` is purely a compile-time idea, and it takes up no space and costs no instructions at run time.

More details in [The Layout of Heap Objects in the GHC Commentary](http://hackage.haskell.org/trac/ghc/wiki/Commentary/Rts/Storage/HeapObjects).
    