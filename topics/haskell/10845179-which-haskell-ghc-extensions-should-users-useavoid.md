
# Which Haskell (GHC) extensions should users use/avoid?

## Question
        
I have had the experience a few times now of having GHC tell me to use an extension, only to discover that when in using that extension I have made code far more complex when a simple refactor would have allowed me to stick with Haskell 98 (now [2010](http://www.haskell.org/onlinereport/haskell2010/haskell.html#haskellli2.html)) and have a more straightforward solution.

On the other hand, there are also times when GADT's or Rank2Types (rarely RankNTypes) make for much less work and much cleaner code.

Which extensions tend generally to obscure the possibility of a better design, and which generally improve it? If there are some that do both, what should a user look for (be sure it true or not true of the solution they are intending) before deciding to use that extension?

(See also [Should I use GHC Haskell extensions or not?](https://stackoverflow.com/questions/801785/should-i-use-ghc-haskell-extensions-or-not))

## Answer
        
An ad hoc list of morally "good" extensions, and morally "bad" ones - this is an aesthetic judgement!

**The Good**

*   GADTs
*   Parallel list comprehensions
*   Pattern guards
*   Monad comprehensions
*   Tuple sections
*   Record wild cards
*   Empty data decls
*   Existential types
*   Generalized new type deriving
*   MPTCs + FDs
*   Type families
*   Explicit quantification
*   Higher rank polymorphism
*   Lexically scoped tyvars
*   Bang Patterns

**The Bad**

*   SQL comprehensions
*   Implicit parameters

**The Ugly** (but necessary)

*   Template Haskell
*   Unboxed types and tuples
*   Undecidable, overlapping and incoherent instances -- usually means you have a misdesign.

**Not sure**

*   Arrow notation
*   View patterns
