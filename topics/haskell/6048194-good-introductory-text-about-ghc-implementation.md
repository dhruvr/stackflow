
# Good introductory text about GHC implementation?

## Question
        
When programming in Haskell (and especially when solving Project Euler problems, where suboptimal solutions tend to stress the CPU or memory needs) I'm often puzzled why the program behaves the way it is. I look at profiles, try to introduce some strictness, chose another data structure, ... but mostly it's groping in the dark, because I lack a good intuition.

Also, while I know how Lisp, Prolog and imperative languages are typically implemented, I have no idea about implementing a lazy language. I'm a bit curious too.

Hence I would like to know more about the whole chain from program source to execution model.

Things I wonder about:

*   what typical optimizations are applied?
    
*   what is the execution order when there are multiple candidates for evaluation (while I know it's driven from the needed outputs, there may still be big performance differences between first evaluating A and then B, or evaluating B first to detect that you don't need A at all)
    
*   how are thunks represented?
    
*   how are the stack and the heap used?
    
*   what is a CAF? (profiling indicates sometimes that the hotspot is there, but I have no clue)

## Answer
        
The majority of the technical information about the architecture and approach of the GHC system is in their wiki. I'll link to the key pieces, and some related papers that people may not know about.

**What typical optimizations are applied?**

The key paper on this is: [_A transformation-based optimiser for Haskell_](http://research.microsoft.com/~simonpj/Papers/comp-by-trans-scp.ps.gz), SL Peyton Jones and A Santos, 1998, which describes the model GHC uses of applying type-preserving transformations (refactorings) of a core Haskell-like language to improve time and memory use. This process is called "simplification".

Typical things that are done in a Haskell compiler include:

*   Inlining;
*   Beta reduction;
*   Dead code elimination;
*   Transformation of conditions: case-of-case, case elimiation.
*   Unboxing;
*   Constructed product return;
*   Full laziness transformation;
*   Specialization;
*   Eta expansion;
*   Lambda lifting;
*   Strictness analysis.

And sometimes:

*   The static argument transformation;
*   Build/foldr or stream fusion;
*   Common sub-expression elimination;
*   Constructor specialization.

The above-mentioned paper is the key place to start to understand most of these optimizations. Some of the simpler ones are given in the earlier book, _[Implementing Functional Languages](http://research.microsoft.com/en-us/um/people/simonpj/papers/pj-lester-book/)_, Simon Peyton Jones and David Lester.

**What is the execution order when there are multiple candidates for evaluation**

Assuming you're on a uni-processor, then the answer is "some order that the compiler picks statically based on heuristics, and the demand pattern of the program". If you're using speculative evaluation via sparks, then "some non-deterministic, out-of-order execution pattern".

In general, to see what the execution order is, look at the core, with, e.g. the [ghc-core](http://hackage.haskell.org/package/ghc-core) tool. An [introduction to Core](http://book.realworldhaskell.org/read/profiling-and-optimization.html#id679074) is in the RWH chapter on optimizations.

**How are thunks represented?**

Thunks are represented as heap-allocated data with a code pointer.

![Heap object](https://i.stack.imgur.com/hU6HB.png)

See [the layout of heap objects](http://hackage.haskell.org/trac/ghc/wiki/Commentary/Rts/Storage/HeapObjects). Specifically, see [how thunks are represented](http://hackage.haskell.org/trac/ghc/wiki/Commentary/Rts/Storage/HeapObjects#Thunks).

**How are the stack and the heap used?**

As determined by [the design of the Spineless Tagless G-machine](http://research.microsoft.com/en-us/um/people/simonpj/papers/spineless-tagless-gmachine.ps.gz#26pub=34), specifically, with many modifications since that paper was released. Broadly, the execution model:

*   (boxed) objects are allocated on the global heap;
*   every [thread object has a stack](http://hackage.haskell.org/trac/ghc/wiki/Commentary/Rts/Storage/Stack), consisting of frames with the same layout as heap objects;
*   when you make a function call, you push values onto the stack and jump to the function;
*   if the code needs to allocate e.g. a constructor, that data is placed on the heap.

To deeply understand the stack use model, see ["Push/Enter versus Eval/Apply"](http://research.microsoft.com/en-us/um/people/simonpj/papers/eval-apply/).

**What is a CAF?**

A "Constant Applicative Form". E.g. a top level constant in your program allocated for the lifetime of your program's execution. Since they're allocated statically, they have to be [treated specially by the garbage collector](http://hackage.haskell.org/trac/ghc/wiki/Commentary/Rts/Storage/GC/CAFs?redirectedfrom=Commentary/Rts/Storage/CAFs).

* * *

_References and further reading_:

*   [The GHC Commentary](http://hackage.haskell.org/trac/ghc/wiki/Commentary)
*   [The Spinless Tagless G-machine](http://research.microsoft.com/en-us/um/people/simonpj/papers/spineless-tagless-gmachine.ps.gz#26pub=34)
*   [Compilation via Transformation](http://research.microsoft.com/~simonpj/Papers/comp-by-trans-scp.ps.gz)
*   [Push/Enter vs Eval/Apply](http://research.microsoft.com/en-us/um/people/simonpj/papers/eval-apply/)
*   [Unboxed Values as First-Class Citizens](http://www.haskell.org/ghc/docs/papers/unboxed-values.ps.gz)
*   [Secrets of the Inliner](http://www.research.microsoft.com/~simonpj/Papers/inlining/index.htm)
*   [Runtime Support for Multicore Haskell](http://research.microsoft.com/apps/pubs/default.aspx?id=79856)
