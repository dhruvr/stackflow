
# How are mutable arrays implemented in Haskell?

## Question
        
I've read many research papers on this topic, and they usually argue that arrays are implemented using Monads. But none of these papers gave a clear definition of how the "type" Array itself should be defined, they only gave definitions for the functions using monads to access or modify this type. How are arrays, having O(1) time to access or modify an indexed element, implemented in Haskell ?! (such as STUArray and MArray)

## Answer
        
> How are arrays, having O(1) time to access or modify an indexed element, implemented in Haskell

They are implemented via primitive operations in the runtime system for memory reads and writes.

The safety of the side effecting action of destructively writing to memory is ensured via the use of monads to linearize access to the mutable state.

Looking at the [`primitive`](http://hackage.haskell.org/package/primitive) package for Haskell arrays (in `IO` or `ST`), you can see that the implementations is in terms of [GHC's _primops_](http://hackage.haskell.org/trac/ghc/wiki/Commentary/PrimOps):

    -- | Create a new mutable array of the specified size and initialise all
    -- elements with the given value.
    newArray :: PrimMonad m => Int -> a -> m (MutableArray (PrimState m) a)
    newArray (I# n#) x = primitive
       (\s# -> case newArray# n# x s# of
                 (# s'#, arr# #) -> (# s'#, MutableArray arr# #))
    
    -- | Read a value from the array at the given index.
    readArray :: PrimMonad m => MutableArray (PrimState m) a -> Int -> m a
    readArray (MutableArray arr#) (I# i#) = primitive (readArray# arr# i#)
    
    -- | Write a value to the array at the given index.
    writeArray :: PrimMonad m => MutableArray (PrimState m) a -> Int -> a -> m ()
    writeArray (MutableArray arr#) (I# i#) x = primitive_ (writeArray# arr# i# x)
    

That is, in terms of:

*   newArray#
*   readArray#
*   writeArray#

which are primitive (hardware accelerated ;) services for operating on memory provided by the language runtime.

Mechanisms for giving type safety to destructive memory effects were introduced to Haskell by the Launchbury and Peyton-Jones paper, [_Lazy Functional State Threads_](http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.50.3299), which introduces the `ST` monad and primitives for mutable arrays.
