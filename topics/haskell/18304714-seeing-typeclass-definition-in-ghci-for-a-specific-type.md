
# Seeing Typeclass definition in ghci for a specific type

## Question
        
Is there a way to see Typeclass definition in ghci for a specific type?

For example, `Maybe` is defined like this:

    instance Functor Maybe where  
         fmap f (Just x) = Just (f x)  
         fmap f Nothing = Nothing  
    

Can I see this in ghci ?

When, I use `:info` in ghci, I get this:

    Prelude> :i Maybe
    data Maybe a = Nothing | Just a     -- Defined in `Data.Maybe'
    instance Eq a => Eq (Maybe a) -- Defined in `Data.Maybe'
    instance Monad Maybe -- Defined in `Data.Maybe'
    instance Functor Maybe -- Defined in `Data.Maybe'
    instance Ord a => Ord (Maybe a) -- Defined in `Data.Maybe'
    instance Read a => Read (Maybe a) -- Defined in `GHC.Read'
    instance Show a => Show (Maybe a) -- Defined in `GHC.Show'
    

In the above output, I want to see how it is defined in Data.Maybe as an instance for `Functor`. Anyway to see that in ghci ?

## Answer
        
No, it's not possible – not just for instances but for anything. GHC only registers the compiled version of a package, so the source code generally won't be available to ghci.

Probably, most often you'll be using stuff from Hackage; in that case it's very simple to find the source code of such instances by [hoogling the module](http://www.haskell.org/hoogle/?hoogle=Data.Maybe), locating the class or data declaration, and clicking on [source](http://hackage.haskell.org/packages/archive/base/latest/doc/html/src/Data-Maybe.html#Maybe).

When you don't have internet access or whatever else reason you can't hoogle online, you first need to find out in what _package_ the module is included. The easiest way to do that:

> $ ghc-pkg find-module Data.Maybe  
> /usr/local/haskell/lib/ghc-7.6.2/package.conf.d  
>    base-4.6.0.1  
>    haskell2010-1.1.1.0  
> ~/.ghc/x86_64-linux-7.6.2/package.conf.d

Then, as I said, GHC doesn't know where the source code to these packages is located – in fact it might not even be available on your system! But if you've installed the package (or one that depends on it) with `cabal install`, it will be there, by default in `~/.cabal/packages/hackage.haskell.org/PᴀᴄᴋᴀɢᴇNᴀᴍᴇ` (as a compressed archive, but that's not a big hurdle). Within the package project folder, you can simply locate the module via the directory structure, which represents the module hierarchy.

Other packages, like your example of `Data.Maybe` (package `haskell2010`), may have come right with your installation of GHC, e.g. the Haskell platform. In that case, I believe the easiest thing is to search there for the Haddock documentation file. In my case,

> $ find /usr/local/haskell -name 'Data-Maybe.html' | head -n1 | xargs firefox

That'll open up the equivalent to what hoogle links you to (but on your local HD), where you can also browse the source code in a user-friendly way.
