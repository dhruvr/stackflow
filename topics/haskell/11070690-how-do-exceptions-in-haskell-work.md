
# How do exceptions in Haskell work?

## Question
        
In GHCi:

    Prelude> error (error "")
    *** Exception: 
    Prelude> (error . error) ""
    *** Exception: *** Exception: 
    

Why isn't the first one a nested exception?

## Answer
        
The answer is that this is the (somewhat surprising) semantics of _imprecise exceptions_

When pure code can be shown to evaluate to a _set_ of exceptional values (i.e. the value of `error` or `undefined`, and explicitly _not_ the kind of exceptions [generated in IO](http://www.haskell.org/ghc/docs/latest/html/libraries/base/Control-Exception.html)), then the language permits any value of that set to be returned. Exceptional values in Haskell are more like `NaN` in floating point code, rather than control-flow based exceptions in imperative languages.

An occasional gotcha for even advanced Haskellers is a case such as:

     case x of
       1 -> error "One"
       _ -> error "Not one"
    

Since the code evaluates to a set of exceptions, GHC is free to pick one. With optimizations on, you may well find this always evaluates to "Not one".

Why do we do this? Because otherwise we'd overly constrain the evaluation order of the language, e.g. we would have to fix a deterministic result for:

     f (error "a") (error "b")
    

by for example, requiring that it be evaluated left-to-right if error values are present. Very un-Haskelly!

Since we don't want to cripple the optimizations that can be done on our code just to support `error`, the solution is to specify that the result is a non-deterministic choice from the set of exceptional values: imprecise exceptions! In a way, all exceptions are returned, and one is chosen.

Normally, you don't care - an exception is an exception - unless you care about the string inside the exception, in which case using `error` to debug is highly confusing.

* * *

References: [A semantics for imprecise exceptions](http://research.microsoft.com/en-us/um/people/simonpj/papers/imprecise-exn.htm), Simon Peyton Jones, Alastair Reid, Tony Hoare, Simon Marlow, Fergus Henderson. Proc Programming Languages Design and Implementation (PLDI'99), Atlanta. ([PDF](http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.50.1525&rep=rep1&type=pdf))
