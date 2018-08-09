
# Is there a visual modeling language or style for the functional programming paradigm?

## Question
        
UML is a standard aimed at the modeling of software which will be written in OO languages, and goes hand in hand with Java. Still, could it possibly be used to model software meant to be written in the functional programming paradigm? Which diagrams would be rendered useful given the embedded visual elements?

Is there a modeling language aimed at functional programming, more specifically Haskell? What tools for putting together diagrams would you recommend?

### Edited by OP Sept 02, 2009:

What I'm looking for is the most visual, lightest representation of what goes on in the code. Easy to follow diagrams, visual models not necessarily aimed at other programmers. I'll be developing a game in Haskell very soon but because this project is for my graduation conclusion work I need to introduce some sort of formalization of the proposed solution. I was wondering if there is an equivalent to the UML+Java standard, but for Haskell. Should I just stick to storyboards, written descriptions, non-formalized diagrams (some shallow flow-chart-like images), non-formalized use case descriptions?

### Edited by jcolebrand June 21, 2012:

Note that the asker originally wanted a visual metphor, and now that we've had three years, we're looking for more/better tools. None of the original answers really addressed the concept of "visual metaphor design tool" so ... that's what the new bounty is looking to provide for.

## Answer
        
We use theorem provers to do formal modelling (with verification), such as Isabelle or Coq. Sometimes we use domain specific languages (e.g. Cryptol) to do the high level design, before deriving the "low level" Haskell implementation.

Often we just use Haskell as the modelling language, and derive the actual implementation via rewriting.

QuickCheck properties also play a part in the design document, along with type and module decompositions.
