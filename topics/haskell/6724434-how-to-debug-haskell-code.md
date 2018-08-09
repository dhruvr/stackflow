
# How to debug Haskell code?

## Question
        
I have a problem. I wrote a big Haskell program, and it always works with small input. Now, when I want to test it and generate a bigger input, I always get the message:

    HsProg: Prelude.head: empty list
    

I use `Prelude.head` many times. What can I do to find out more or get a better error output to get the code line in which it happens?

## Answer
        
The GHCi option `-fbreak-on-exception` can be useful. Here's an example debugging session. First we load our file into GHCi.

    $ ghci Broken.hs
    GHCi, version 7.0.2: http://www.haskell.org/ghc/  :? for help
    Loading package ghc-prim ... linking ... done.
    Loading package integer-gmp ... linking ... done.
    Loading package base ... linking ... done.
    Loading package ffi-1.0 ... linking ... done.
    [1 of 1] Compiling Main             ( Broken.hs, interpreted )
    Ok, modules loaded: Main.
    

Now, we turn on `-fbreak-on-exceptions` and trace our expression (`main` in this case for the whole program).

    *Main> :set -fbreak-on-exception
    *Main> :trace main
    Stopped at <exception thrown>
    _exception :: e = _
    

We've stopped at an exception. Let's try to look at the code with `:list`.

    [<exception thrown>] *Main> :list
    Unable to list source for <exception thrown>
    Try :back then :list
    

Because the exception happened in `Prelude.head`, we can't look at the source directly. But as GHCi informs us, we can go `:back` and try to list what happened before in the trace.

    [<exception thrown>] *Main> :back
    Logged breakpoint at Broken.hs:2:23-42
    _result :: [Integer]
    [-1: Broken.hs:2:23-42] *Main> :list
    1  
    2  main = print $ head $ filter odd [2, 4, 6]
    3  
    

In the terminal, the offending expression `filter odd [2, 4, 6]` is highlighted in bold font. So this is the expression that evaluated to the empty list in this case.

For more information on how to use the GHCi debugger, see [the GHC User's Guide](http://www.haskell.org/ghc/docs/latest/html/users_guide/ghci-debugger.html).
