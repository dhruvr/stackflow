
# What&apos;s the status of multicore programming in Haskell?

## Question
        
What's the status of multicore programming in Haskell? What projects, tools, and libraries are available now? What experience reports have there been?

## Answer
        
In the 2009-2012 period, the following things have happened:

2012:

*   From 2012, the parallel Haskell status updates began appearing in the [Parallel Haskell Digest](http://www.well-typed.com/blog/65).

2011:

*   [Parallel and Concurrent Programming in Haskell](http://community.haskell.org/~simonmar/par-tutorial.pdf), a tutorial. version 1.1 released by Simon Marlow
*   [Haskell and parallelism](http://www.economist.com/node/18750706?story_id=18750706), mentioned in an article in the Economist magazine, Jun 2nd 2011.
*   [Parallel tree scans via composition](http://conal.net/blog/posts/parallel-tree-scanning-by-composition/), an article by Conal Elliott
*   [Numeric Haskell](http://www.haskell.org/haskellwiki/Numeric_Haskell:_A_Repa_Tutorial), a tutorial on parallel array programming with Repa, released
*   Works has begun on extending GHC eventlog and Threadscope to support multi-process or distributed Haskell systems
*   [Parallel Haskell Digest: Edition 2](http://www.well-typed.com/blog/53).
*   [The par-monad package](http://hackage.haskell.org/package/monad-par) and [a monad for deterministic parallelism](http://community.haskell.org/~simonmar/papers/monad-par.pdf), Simon Marlow -- more control over pure parallelism than strategies/par/pseq.
*   [Cloud Haskell](http://research.microsoft.com/en-us/um/people/simonpj/papers/parallel/remote.pdf): Erlang-style message passing between distributed Haskell nodes.
*   [Parallel Haskell: Embracing Diversity](http://skillsmatter.com/podcast/scala/talk-by-haskell-expert-simon-peyton-jones/js-1434), a talk by SPJ.
*   [Real time edge detection in parallel Haskell](http://disciple-devel.blogspot.com/2011/03/real-time-edge-detection-in-haskell.html)
*   [Parallel Haskell Digest: news on parallel Haskell](http://www.well-typed.com/blog/52)
*   [Composable parallel scanning](http://conal.net/blog/posts/composable-parallel-scanning/)
*   [Haskell-MPI](http://hackage.haskell.org/package/haskell-mpi-1.0.0) is released

2010:

*   [Parallel futures](http://ghcmutterings.wordpress.com/2010/08/20/parallel-programming-in-haskell-with-explicit-futures/) for Haskell, in GHC.
*   The [Orc language](http://corp.galois.com/blog/2010/6/14/orc-in-haskell-now-on-hackage.html), for concurrent job scheduling and scripting, was released.
*   A [new scalable thread event manager](http://www.serpentine.com/bos/files/ghc-event-manager.pdf) was merged into GHC.
*   An [improved approach to parallel sparks](http://www.haskell.org/~simonmar/papers/strategies.pdf) and strategies was developed.
*   The [Nikola EDSL](http://www.eecs.harvard.edu/~mainland/publications/mainland10nikola.pdf) for embedding GPU programs in Haskell was developed.
*   The [LLVM backend for GHC](http://www.cse.unsw.edu.au/~chak/papers/TC10.html) was merged in, with good performance improvements.
*   [ghc 6.12.x series: with parallel performance improvements](http://article.gmane.org/gmane.comp.lang.haskell.general/17678)
*   Microsoft announces [2 years of funding to support commercial users of Parallel Haskell](http://blog.well-typed.com/2010/04/parallel-haskell-2-year-project-to-push-real-world-use/)
*   [Google published their experience report on the use of Haskell](http://www.icfpconference.org/icfp2010/accepted_papers.html) ([PDF](http://k1024.org/~iusty/papers/icfp10-haskell-reagent.pdf))
*   Intel announced [the Concurrent Collections for Haskell library](http://software.intel.com/en-us/blogs/2010/05/27/announcing-intel-concurrent-collections-for-haskell-01/), including [scalability numbers](http://software.intel.com/en-us/blogs/2010/06/07/parallel-performance-in-intel-concurrent-collections-for-haskell-an-in-depth-example/) \-\- scaling results [for 32 and 48 cores](http://software.intel.com/en-us/blogs/2010/06/24/haskell-cnc-new-paper-available-tests-on-32-and-48-cores/)
*   Sun/Oracle [bought us a machine](http://hackage.haskell.org/trac/ghc/wiki/OpenSPARC) and funded work on [improving parallel performance](http://ghcsparc.blogspot.com/).
*   Recent updates [to the status of Data Parallelism in Haskell](http://www.youtube.com/watch?v=NWSZ4c9yqW8)
*   MSR released [ThreadScope](http://research.microsoft.com/en-us/projects/threadscope/), a graphical profiler for parallel Haskell programs
*   The GHC runtime [got extensively tuned for sparks and futures](http://ghcmutterings.wordpress.com/2009/03/03/new-paper-runtime-support-for-multicore-haskell/)
*   There was a good [discussion on additional ways to improve parallel performance](http://ghcmutterings.wordpress.com/2010/01/25/yielding-more-improvements-in-parallel-performance/)
*   A collection of [reading material on parallelism in Haskell](http://donsbot.wordpress.com/2009/09/03/parallel-programming-in-haskell-a-reading-list/) to help you get started
*   The [Snap guys are getting 45k req/sec on their 4 way box](http://gregorycollins.net/posts/2010/03/12/attoparsec-iteratee#comment-39671374), by using all the cores.
*   Even the [Erlang guys are taking notice](http://orbitz-erlang.blogspot.com/2009/09/impressed-with-haskells-concurrency.html).
*   Meanwhile, [there is work to make the IO manager more scalable](http://www.serpentine.com/blog/2009/12/17/making-ghcs-io-manager-more-scalable/) \-\- now with [a paper on the design](http://www.serpentine.com/bos/files/ghc-event-manager.pdf) :: PDF.
*   We're out [there teaching people too](http://www.slideshare.net/bos31337/bayfp-concurrent-and-multicore-haskell) .. [all](http://donsbot.wordpress.com/2010/06/01/open-source-bridge-talk-multicore-haskell-now/) .. [over](http://vimeo.com/channels/haskell#6680185) .. [the](http://ulf.wiger.net/weblog/2008/02/29/satnam-singh-declarative-programming-techniques-for-many-core-architectures/) ... [place](http://blip.tv/file/324976).
*   Starling Software [wrote about their real time, multicore financial trading system in Haskell](http://www.starling-software.com/misc/icfp-2009-cjs.pdf).
*   Ericsson published a [parallel language for DSP](http://hackage.haskell.org/package/feldspar-language) based on, and written in Haskell
*   Galois published an implementation of [Orc](http://hackage.haskell.org/package/orc), a concurrent workflow language, in Haskell.
*   And a [new library](http://repa.ouroborus.net/) for [fast regular, parallel arrays appeared](http://hackage.haskell.org/package/repa)
*   And [Haskell continues to do well on the quad-core shootout](http://shootout.alioth.debian.org/u64q/which-programming-languages-are-fastest.php#table).
*   [Snap](http://www.haskell.org/pipermail/haskell-cafe/2010-May/078005.html), a multicore-enabled scalable web server with great performance numbers
*   [haskell-torrent](http://jlouisramblings.blogspot.com/2009/12/concurrency-bittorrent-clients-and.html) \- benchmarking a mulitcore-enabled bittorrent client in Haskell
*   [Haskell code was published](http://scyourway.supercomputing.org/conference/view/spost112_1) at Supercomputing 09 -- our first appearance at SC!
