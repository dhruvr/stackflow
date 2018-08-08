
# Reading GHC Core

## Question
      
Core is GHC's intermediate language. Reading Core can help you better understand the performance of your program. Someone asked me for documentation or tutorials on reading Core, but I couldn't find much.

What documentation is available for reading GHC Core?

Here's what I've found so far:

*   [Write Haskell as fast as C: exploiting strictness, laziness and recursion](http://donsbot.wordpress.com/2008/05/06/write-haskell-as-fast-as-c-exploiting-strictness-laziness-and-recursion/)
*   [Haskell as fast as C: working at a high altitude for low level performance](http://donsbot.wordpress.com/2008/06/04/haskell-as-fast-as-c-working-at-a-high-altitude-for-low-level-performance/)
*   [RWH: Chapter 25. Profiling and optimization](http://book.realworldhaskell.org/read/profiling-and-optimization.html)
*   [High-Performance Haskell talk at CUFP](http://blog.johantibell.com/2010/09/slides-from-my-high-performance-haskell.html) (slide 65-80)
## Answer
      
GHC Core is the [System FC](http://research.microsoft.com/~simonpj/papers/ext-f/) language into which all Haskell is translated. The (approximate) grammar for Core is given by:

![enter image description here](https://i.stack.imgur.com/RP6fQ.png)

Core is closely related to the simpler and better known [System F](http://en.wikipedia.org/wiki/System_F). All [transformations GHC does on the Core level](http://research.microsoft.com/en-us/um/people/simonpj/papers/comp-by-trans-scp.ps.gz) are type-preserving refactorings of this Core representation, to improve performance. And, not so well known, you can write directly in Core to program GHC.

GHC Core fits in the compiler pipeline (as it was in 2002, sans-LLVM and CMM):

![enter image description here](https://i.stack.imgur.com/PtsCf.png)

The primary documents to learn about GHC Core are:

*   **[An External Representation for the GHC Core Language](http://www.haskell.org/ghc/docs/papers/core.ps.gz), Tolmach, 2001**
*   [ghc/compiler/CoreSyn](https://github.com/ghc/ghc/blob/master/compiler/coreSyn/CoreSyn.hs#L141), the GHC definition itself
*   **[Secrets of the Glasgow Haskell Compiler inliner](http://research.microsoft.com/en-us/um/people/simonpj/papers/inlining/inline-jfp.ps.gz)**, Peyton Jones and Marlow, 1999. Core is described in Section 2.3, including details on the occurrence analysis annotations.
*   **[A transformation-based optimiser for Haskell](http://research.microsoft.com/pubs/67064/comp-by-trans-scp.ps.gz)**, Peyton Jones and Santos, 1998. Core is described in S3, including a discussion of polymorphism and operational readings of Core.

Related material that can aid understanding:

*   The [GHC -fext-core output](http://www.haskell.org/ghc/docs/latest/html/users_guide/ext-core.html)
*   I spent a lot of time learning Core by reading GHC source. Some is described in my [undergraduate thesis](https://web.archive.org/web/20170118091715/https://www.cse.unsw.edu.au/~pls/thesis/dons-thesis.ps.gz) from 2002, from page 16.
*   From using the [ghc-core](http://hackage.haskell.org/package/ghc-core) tool, to generate Core in a format I find pleasing.

Core in turn is translated into STG code, which looks something like:

![enter image description here](https://i.stack.imgur.com/Gaj9X.png)

The funny names in Core are encoded in the "Z-encoding":

![enter image description here](https://i.stack.imgur.com/J9pqR.png)

GHC Core's types and kinds (from Tolmach's paper):

![enter image description here](https://i.stack.imgur.com/eNvd2.png)

Finally, GHC's [primops](https://ghc.haskell.org/trac/ghc/wiki/Commentary/PrimOps) appear regularly in GHC Core output, when you have optimized your Haskell down to the basic instructions GHC knows about. The primop set is given as a set of Core functions in [a pre-processed file.](https://ghc.haskell.org/trac/ghc/browser/ghc/compiler/prelude/primops.txt.pp#L12)
    