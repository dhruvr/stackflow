
# How to hack GHCi (or Hugs) so that it prints Unicode chars unescaped?

## Question
        
Look at the problem: Normally, in the interactive Haskell environment, non-Latin Unicode characters (that make a part of the results) are printed escaped, even if the locale allows such characters (as opposed to direct output through `putStrLn`, `putChar` which looks fine and readable)--the examples show GHCi and Hugs98:

    $ ghci
    GHCi, version 7.0.1: http://www.haskell.org/ghc/  :? for help
    Prelude> "hello: привет"
    "hello: \1087\1088\1080\1074\1077\1090"
    Prelude> 'Я'
    '\1071'
    Prelude> putStrLn "hello: привет"
    hello: привет
    Prelude> :q
    Leaving GHCi.
    $ hugs -98
    __   __ __  __  ____   ___      _________________________________________
    ||   || ||  || ||  || ||__      Hugs 98: Based on the Haskell 98 standard
    ||___|| ||__|| ||__||  __||     Copyright (c) 1994-2005
    ||---||         ___||           World Wide Web: http://haskell.org/hugs
    ||   ||                         Bugs: http://hackage.haskell.org/trac/hugs
    ||   || Version: September 2006 _________________________________________
    
    Hugs mode: Restart with command line option +98 for Haskell 98 mode
    
    Type :? for help
    Hugs> "hello: привет"
    "hello: \1087\1088\1080\1074\1077\1090"
    Hugs> 'Я'
    '\1071'
    Hugs> putStrLn "hello: привет"
    hello: привет
    
    Hugs> :q
    [Leaving Hugs]
    $ locale
    LANG=ru_RU.UTF-8
    LC_CTYPE="ru_RU.UTF-8"
    LC_NUMERIC="ru_RU.UTF-8"
    LC_TIME="ru_RU.UTF-8"
    LC_COLLATE="ru_RU.UTF-8"
    LC_MONETARY="ru_RU.UTF-8"
    LC_MESSAGES="ru_RU.UTF-8"
    LC_PAPER="ru_RU.UTF-8"
    LC_NAME="ru_RU.UTF-8"
    LC_ADDRESS="ru_RU.UTF-8"
    LC_TELEPHONE="ru_RU.UTF-8"
    LC_MEASUREMENT="ru_RU.UTF-8"
    LC_IDENTIFICATION="ru_RU.UTF-8"
    LC_ALL=
    $ 
    

We can guess that it's because [`print`](http://hackage.haskell.org/packages/archive/base/latest/doc/html/Prelude.html#v:print) and [`show`](http://hackage.haskell.org/packages/archive/base/latest/doc/html/Prelude.html#v:show) are used to format the result, and these functions do their best to format the data in a canonical, maximally portable way -- so they prefer to escape the strange characters (perhaps, it's even spelled out in a standard for Haskell):

    $ ghci
    GHCi, version 7.0.1: http://www.haskell.org/ghc/  :? for help
    Prelude> show 'Я'
    "'\\1071'"
    Prelude> :q
    Leaving GHCi.
    $ hugs -98
    Type :? for help
    Hugs> show 'Я'
    "'\\1071'"
    Hugs> :q
    [Leaving Hugs]
    $ 
    

But still it would be nice if we knew how to hack GHCi or Hugs to print these characters in the pretty human-readable way, i.e. directly, unescaped. This can be appreciated when using the interactive Haskell environment in educational purposes, for a tutorial/demonstration of Haskell in front of a non-English audience whom you want to show some Haskell on data in their human language.

Actually, it's not only useful for educational purposes but for debugging, as well! When you have functions that are defined on strings representing words of other languages, with non-ASCII characters. So, if the program is language-specific, and only words of another language make sense as the data, and you have functions that are defined only on such words, it's important for debugging in GHCi to see this data.

**To sum up my question:** What ways to hack the existing interactive Haskell environments for a friendlier printing of Unicode in the results are there? ("Friendlier" means even "simpler" in my case: I'd like `print` in GHCi or Hugs to show non-Latin characters the simple direct way as done by `putChar`, `putStrLn`, i.e. unescaped.)

(Perhaps, besides GHCi and Hugs98, I'll also have a look at existing Emacs modes for interacting with Haskell to see if they can present the results in the pretty, unescaped fashion.)

## Answer
        
Option 1 (bad):
---------------

Modify this line of code:

[https://github.com/ghc/packages-base/blob/ba98712/GHC/Show.lhs#L356](https://github.com/ghc/packages-base/blob/ba98712/GHC/Show.lhs#L356)

    showLitChar c s | c > '\DEL' =  showChar '\\' (protectEsc isDec (shows (ord c)) s)
    

And recompile ghc.

Option 2 (lots of work):
------------------------

When GHCi type checks a parsed statement it ends up in `tcRnStmt` which relies on `mkPlan` (both in [https://github.com/ghc/ghc/blob/master/compiler/typecheck/TcRnDriver.lhs](https://github.com/ghc/ghc/blob/master/compiler/typecheck/TcRnDriver.lhs)). This attempts to type check several variants of the statement that was typed in including:

    let it = expr in print it >> return [coerce HVal it]
    

Specifically:

    print_it  = L loc $ ExprStmt (nlHsApp (nlHsVar printName) (nlHsVar fresh_it))
                                          (HsVar thenIOName) placeHolderType
    

All that might need to change here is `printName` (which binds to `System.IO.print`). If it instead bound to something like `printGhci` which was implemented like:

    class ShowGhci a where
        showGhci :: a -> String
        ...
    
    -- Bunch of instances?
    
    instance ShowGhci Char where
        ...  -- The instance we want to be different.
    
    printGhci :: ShowGhci a => a -> IO ()
    printGhci = putStrLn . showGhci
    

Ghci could then change what is printed by bringing different instances into context.
