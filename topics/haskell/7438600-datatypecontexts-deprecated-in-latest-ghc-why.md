
# DatatypeContexts Deprecated in Latest GHC: Why?

## Question
        
I was just doing some Haskell development and I recompiled some old code on a new version of GHC:

    The Glorious Glasgow Haskell Compilation System, version 7.2.1
    

And when I did I received the following error:

> Warning: -XDatatypeContexts is deprecated: It was widely considered a misfeature, and has been removed from the Haskell language.

That appears when you have code in the following format:

    data Ord a => MyType a
        = ConstructorOne a
        = ConstructorTwo a a
    

My question is: Why was this feature deprecated in the first place and what am I supposed to do instead to achieve the same or similar functionality?

## Answer
        
It's deprecated because it _was_ a misfeature, and didn't actually _have_ any useful functionality! All it did was force a bunch of extra constraints in other locations. In particular, when pattern matching on such a type, you'd be forced to add a constraint, rather than (as one might initially hope) get access to a context, based on the knowledge that one must have been available to construct the value in the first place.

The "replacement", which actually works the other way and tracks the known contexts for you, is to [use GADT-style declarations instead](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#generalised-algebraic-data-types-gadts):

    data MyType a where
        ConstructorOne :: Ord a => a -> MyType a
        ConstructorTwo :: Ord a => a -> a -> MyType a
    

GADTs in general are more flexible in many other ways as well, but for this particular case what happens is that _creating_ a value needs the `Ord` constraint, which is then carried along with the value, and pattern matching on the constructor pulls it back out. So you don't even need the context on the functions using it, because you know that by virtue of expecting something of type `MyType a`, you'll get an `Ord a` constraint with it.
