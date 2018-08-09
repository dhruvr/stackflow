
# Pattern matching identical values

## Question
        
I just wondered whether it's possible to match against the same values for multiple times with the pattern matching facilities of functional programming languages (Haskell/F#/Caml).

Just think of the following example:

    plus a a = 2 * a
    plus a b = a + b
    

The first variant would be called when the function is invoked with two similar values (which would be stored in `a`).

A more useful application would be this (simplifying an AST).

    simplify (Add a a) = Mult 2 a
    

But Haskell rejects these codes and warns me of _conflicting definitions for `a`_ \- I have to do explicit case/if-checks instead to find out whether the function got identical values. Is there any trick to indicate that a variable I want to match against will occur multiple times?

## Answer
        
This is called a _nonlinear pattern_. There have been several threads on the haskell-cafe mailing list about this, not long ago. Here are two:

[http://www.mail-archive.com/haskell-cafe@haskell.org/msg59617.html](http://www.mail-archive.com/haskell-cafe@haskell.org/msg59617.html)

[http://www.mail-archive.com/haskell-cafe@haskell.org/msg62491.html](http://www.mail-archive.com/haskell-cafe@haskell.org/msg62491.html)

Bottom line: it's not impossible to implement, but was decided against for sake of simplicity.

By the way, you do not need `if` or `case` to work around this; the (slightly) cleaner way is to use a guard:

    a `plus` b
      | a == b = 2*a
      | otherwise = a+b
