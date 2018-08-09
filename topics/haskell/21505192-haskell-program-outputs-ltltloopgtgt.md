
# Haskell program outputs `&lt;&lt;loop&gt;&gt;`

## Question
        
I wrote a Haskell program that preforms a binary search on a list. At least that's what I thought it does. When I compiled the program with `ghc v7.6.3` and ran the program I got the following output:

    progname: <<loop>>
    

What on earth does this output mean? Does it mean I had an infinite loop that `ghc` optimized away? How am I supposed to debug this?

## Answer
        
As several of the comments have said, this is the Haskell RTS detecting an infinite loop at run-time. It [cannot _always_ detect such loops](https://en.wikipedia.org/wiki/Halting_problem), but in simple cases it can.

For example,

    x = x + 1
    

will compile just fine, but provoke an exception at run-time. (Incidentally, this is an _exception_ \- in particular, you can _catch_ it if you want. But you probably don't "want".)

So why does GHC even let this compile? Well, because if I replace `+` with, say, `:`, then the expression now terminates just fine. (It represents a 1-element circular list.) The compiler can't tell at compile-time what is and is not sensible recursion. The RTS can't always tell at run-time; but when it _can_ tell something's wrong, it'll let you know by throwing an exception at you.
