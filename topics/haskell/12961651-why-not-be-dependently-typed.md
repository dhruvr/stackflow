
# Why not be dependently typed?

## Question
        
I have seen several sources echo the opinion that "Haskell is gradually becoming a dependently-typed language". The implication seems to be that with more and more language extensions, Haskell is drifting in that general direction, but isn't there yet.

There are basically two things I would like to know. The first is, quite simply, what does "being a dependently-typed language" actually _mean_? (Hopefully without being too technical about it.)

The second question is... what's the drawback? I mean, people know we're heading that way, so there must be some advantage to it. And yet, we're not there yet, so there must be some downside stopping people going all the way. I get the impression that the problem is a steep increase in complexity. But, not really understanding what dependent typing is, I don't know for sure.

What I _do_ know is that every time I start reading about a dependently-typed programming language, the text is utterly incomprehensible... Presumably that's the problem. (?)

## Answer
        
Dependent typing is really just the unification of the value and type levels, so you can parametrize values on types (already possible with type classes and parametric polymorphism in Haskell) and you can parametrize types on values (not, strictly speaking, possible yet in Haskell, although `DataKinds` gets very close).

**Edit:** Apparently, from this point forward, I was wrong (see @pigworker's comment). I'll preserve the rest of this as a record of the myths I've been fed. :P

* * *

The issue with moving to full dependent typing, from what I've heard, is that it would break the phase restriction between the type and value levels that allows Haskell to be compiled to efficient machine code with erased types. With our current level of technology, a dependently typed language _must_ go through an interpreter at some point (either immediately, or after being compiled to dependently-typed bytecode or similar).

This is not necessarily a fundamental restriction, but I'm not personally aware of any current research that looks promising in this regard but that has not already made it into GHC. If anyone else knows more, I would be happy to be corrected.
