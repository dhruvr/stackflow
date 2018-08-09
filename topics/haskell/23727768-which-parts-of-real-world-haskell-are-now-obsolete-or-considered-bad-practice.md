
# Which parts of Real World Haskell are now obsolete or considered bad practice?

## Question
        
In the chapter 19 of _Real World Haskell_ a lot of the examples now fail due to the change of `Control.Exception`.

That makes me think maybe some of the stuff in this book is actually obsolete and not worth studying anymore, after all it's been 6 years. My only other reference is _Learn You a Haskell For Great Good_, though it's a great book, it's much more basic compared with RWH.

Can anyone who have read the book before please give some advice on which parts of it are no longer relevant? Especially the chapters in the second half of the book, for example, software transactional memory, concurrent programming, socket programming, etc.

EDIT: This is about the edition of the book that's published on Dec 2008, which is the only known edition as of today (Nov 2017)

## Answer
        
Main issue of RWH
=================

**It's old.** RWH was written at a time version 6.8 of GHC was being used. [6.8](http://www.haskell.org/ghc/docs/6.8.1/html/users_guide/release-6-8-1.html#id3129818) used base version 3.0.x.x. 6.10.1 already used 4.0.0.0, which introduced [many changes](http://www.haskell.org/ghc/docs/6.10.1/html/users_guide/release-6-10-1.html). And that's just the jump from 6.8 to 6.10. The current version of GHC is 7.10. Monads have been changed. There's currently a discussion [to remove `return` from `Monad`](http://thread.gmane.org/gmane.comp.lang.haskell.libraries/25274), so the `Monad` instance in Real World Haskell will really be out of sync with the real world.

That being said, it's still a useful resource for general guidelines. But keep in mind that many libraries changed since its release.

Something you can read along while reading RWH is ["What I Wish I Knew When Learning Haskell" by Stephen Diehl](http://www.stephendiehl.com/what/). It provides additional insight, but be aware, some sections aren't really newcomer friendly.

General remarks
===============

*   Read the comments. They usually contain information whether the given paragraph/section is still relevant and/or working.
*   Read the documentation of the libraries/functions you want to use. Even if you're lazy, know at least the types.

Remarks to chapters
===================

This is just a quick overview of some of the things that I noticed while reading RWH. It's probably incomplete.

Chapter 2. Types and Functions **_vs the FTP_**
-----------------------------------------------

_Since GHC 7.10_.

The type of [`null`](http://book.realworldhaskell.org/read/types-and-functions.html#myDrop.ghci:null) has been [changed](http://hackage.haskell.org/package/base-4.8.1.0/docs/Prelude.html#v:null) due to the [Foldable-Traversable-Proposal](https://wiki.haskell.org/Foldable_Traversable_In_Prelude). Many other functions such as `foldr`, `foldl` and many other that were previously only defined for `[a]` in the `Prelude` have been replaced with more general `Foldable t => t a` variants.

[Chapter 11. Testing and quality assurance](http://book.realworldhaskell.org/read/testing-and-quality-assurance.html)
---------------------------------------------------------------------------------------------------------------------

_Since Haskell-platform 2010 or late 2008._

Although this is mentioned in a [footnote](http://book.realworldhaskell.org/read/testing-and-quality-assurance.html#ftn.id628218), the QuickCheck library has changed in many ways from version 1 to version 2. For example, `generate` now uses `Gen a` instead of `StdGen`, and the functionality of the old `generate` is in `Test.QuickCheck.Gen.unGen`.

In doubt, check the [documentation](https://hackage.haskell.org/package/QuickCheck).

Chapter 14. Monads & Chapter 15. Programming with monads
--------------------------------------------------------

### Code breaking: `Applicative m => Monad m`

As of GHC 7.10, `Applicative` is now a superclass of `Monad`, something that wasn't planned in 2007.

> In GHC 7.10, `Applicative` will become a superclass of `Monad`, potentially breaking a lot of user code. To ease this transition, GHC now generates warnings when definitions conflict with the Applicative-Monad Proposal ([AMP](https://github.com/quchen/articles/blob/master/applicative_monad.md)).

See [7.8.1 release notes](https://www.haskell.org/ghc/docs/7.4.1/html/users_guide/release-7-4-1.html).

### The `State`/`Writer`/`Reader` monads

In the [Will the real state monad please stand up?](http://book.realworldhaskell.org/read/monads.html#id643643) section, the authors claim

> In order to define a `Monad` instance, we have to provide a proper type constructor as well as definitions for `(>>=)` and `return`. This leads us to the real definition of `State`.
> 
>     -- file: ch14/State.hs
>     newtype State s a = State
>         runState :: s -> (a, s)
>     }
>     

That's no longer true, because `State` and its friends are now implemented via

    type State  s = StateT  s Identity
    type Writer w = WriterT w Identity
    type Reader r = ReaderT r Identity
    

So they're defined by their monad transformer.

[Chapter 17. Interfacing with C: the FFI](http://book.realworldhaskell.org/read/interfacing-with-c-the-ffi.html)
----------------------------------------------------------------------------------------------------------------

The overall chapter is fine, but as one can read in the comments or on [Yuras Shumovich's blog](http://blog.haskell-exists.com/yuras/posts/malloc-free-and-ffi.html), the finalizer part in the following code is bad practise:

    pcre_ptr <- c_pcre_compile pattern (combineOptions flags) errptr erroffset nullPtr
    if pcre_ptr == nullPtr
        then do
            err <- peekCString =<< peek errptr
            return (Left err)
        else do
            reg <- newForeignPtr finalizerFree pcre_ptr -- release with free()
            return (Right (Regex reg str))
    

As `malloc()` should be used with `free()`, `new` with `delete`, `allocate` with `deallocate`, one should always use the correct function.

> TL;DR You should always free memory with the same allocator that allocated it for you.

If a foreign function allocates memory, you should also use the accompanying deallocation function.

[Chapter 19. Error handling](http://book.realworldhaskell.org/read/error-handling.html)
---------------------------------------------------------------------------------------

Error handling changed completely from 6.8 to 6.10, but you noticed that already. Better read the [documentation](http://hackage.haskell.org/package/base-4.7.0.0/docs/Control-Exception.html).

[Chapter 22. Extended Example: Web Client Programming](http://book.realworldhaskell.org/read/extended-example-web-client-programming.html)
------------------------------------------------------------------------------------------------------------------------------------------

Some of the example seem to be broken. Also, there are other HTTP libraries available.

[Chapter 25. Profiling and optimization](http://book.realworldhaskell.org/read/profiling-and-optimization.html)
---------------------------------------------------------------------------------------------------------------

General profiling techniques are still the same, and the example (see below) is a great case study for problems that can occur in your program. But RWH is missing multi-threaded profiling, e.g. via ThreadScope. Also, lazy IO isn't concerned throughout the whole book, as far as I know.

    mean :: [Double] -> Double
    mean xs = sum xs / fromIntegral (length xs)
    

Chapter 24 & Chapter 28 (Concurrent and parallel programming & STM)
-------------------------------------------------------------------

While [Chapter 24. Concurrent and multicore programming](http://book.realworldhaskell.org/read/concurrent-and-multicore-programming.html) and [Chapter 28. Software transactional memory](http://book.realworldhaskell.org/read/software-transactional-memory.html) are still relevant, Simon Marlow's book [Parallel and Concurrent Programming in Haskell](http://chimera.labs.oreilly.com/books/1230000000929/index.html) focuses solely on concurrent and parallel programming and is pretty recent (2013). GPU programming and repa are completely missing in RWH.

[Chapter 26. Advanced library design: building a Bloom filter](http://book.realworldhaskell.org/read/advanced-library-design-building-a-bloom-filter.html)
----------------------------------------------------------------------------------------------------------------------------------------------------------

As with the other chapters, the general guidelines of the design library is still well written and relevant. However, due to some changes (?) concerning `ST`, the result cannot be compiled anymore.

Chapter 27. Network programming
-------------------------------

It's still mostly up to date. After all, network programming doesn't change so easily. However, the code uses deprecated functions `bindSocket` and `sClose`, which should be replaced by `bind` and `close` (preferably via qualified import). Keep in mind that it's very low-level, you might want to use a more specialized high-level library.

[Appendix A. Installing GHC and Haskell libraries](http://book.realworldhaskell.org/read/installing-ghc-and-haskell-libraries.html)
-----------------------------------------------------------------------------------------------------------------------------------

GHC 6.8 was the last version before the Haskell Platform has been introduced. Therefore, the appendix tells you to get GHC and Cabal by hand. Don't. Instead, follow the instructions on the haskell.org [download page](https://www.haskell.org/downloads).

Also, the appendix doesn't tell you about Cabal sandboxes, which were introduced in [Cabal 1.18 and free you from dependency hell](http://coldwa.st/e/blog/2013-08-20-Cabal-sandbox.html). And of course, `stack` is missing completely.

Missing content
===============

Some topics are not discussed in RWH at all. This includes streaming libraries such as [pipes](http://hackage.haskell.org/package/pipes) and [conduit](http://hackage.haskell.org/package/conduit), and also [lenses](http://hackage.haskell.org/package/lens).

There are several resources out there for those topics, but here are some links to introductions to give you an idea what they're about. Also, if you want to use vectors, use the [`vectors`](http://hackage.haskell.org/package/vector) package.

`Control.Applicative`
---------------------

RWH uses `Control.Applicative`'s `(<$>)` at several points, but doesn't explain `Control.Applicative` at all. [LYAH](http://learnyouahaskell.com/functors-applicative-functors-and-monoids) and the [Typeclassopedia](https://wiki.haskell.org/Typeclassopedia#Applicative) contain sections on `Applicative`. Given that `Applicative` is a superclass of `Monad` (see above), it's recommended to learn that class by heart.

Furthermore, several operators of `Control.Applicative` (and the typeclass itself) are now part of the `Prelude`, so make sure that your operators don't clash with `<$>`, `<*>` and others.

Lenses
------

*   [Video by Edward Kmett](https://www.youtube.com/watch?v=cefnmjtAolY&feature=youtu.be&hd=1) (author of `lens`)
*   [Video by Adam Gundry "Lenses: compositional data access and manipulation"](https://skillsmatter.com/skillscasts/4251-lenses-compositional-data-access-and-manipulation)
*   [Introduction and tutorial by Jakub Arnold](http://blog.jakubarnold.cz/2014/07/14/lens-tutorial-introduction-part-1.html)

Streaming libraries
-------------------

*   [Conduit Overview by Michael Snoyman](https://www.fpcomplete.com/user/snoyberg/library-documentation/conduit-overview) (author of `conduit`)
*   [Pipes tutorial by Gabriel Gonzales](http://hackage.haskell.org/package/pipes-4.1.2/docs/Pipes-Tutorial.html) (author of `pipes`, included in the `pipes` package)

Tooling
-------

*   version 1.18 of Cabal, which introduced [sandboxes](http://coldwa.st/e/blog/2013-08-20-Cabal-sandbox.html)
*   [`stack`](http://docs.haskellstack.org/en/stable/README.html), a cross-platform program for developing Haskell projects
*   [`ghc-mod`](https://hackage.haskell.org/package/ghc-mod), a backend for vim, emacs, Sublime Text and other editors

New/missing language extensions and GHC changes
-----------------------------------------------

*   runtime type polymorphism (`:i ($)` has changed tremendously)
*   [`-XTypeInType`](https://ghc.haskell.org/trac/ghc/wiki/DependentHaskell/Phase1)
*   [`-XDataKinds`](https://stackoverflow.com/questions/20558648/what-is-the-datakinds-extension-of-haskell)
*   `-XGADT`
*   `-XRankNTypes`
*   `-XGenericNewtypeDeriving`
*   `-XDeriveFunctor`
*   any other extension that happened after 6.6
