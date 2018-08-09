
# Haskell: read input character from console immediately, not after newline

## Question
        
I've tried this:

    main = do
        hSetBuffering stdin NoBuffering 
        c <- getChar
    

but it waits until the enter is pressed, which is not what I want. I want to read the character immediately after user presses it.

I am using ghc v6.12.1 on Windows 7.

EDIT: workaround for me was moving from GHC to WinHugs, which supports this correctly.

## Answer
        
Might be a bug:

[http://hackage.haskell.org/trac/ghc/ticket/2189](http://hackage.haskell.org/trac/ghc/ticket/2189)

> The following program repeats inputted characters until the escape key is pressed.
> 
>     import IO
>     import Monad
>     import Char
>     
>     main :: IO ()
>     main = do hSetBuffering stdin NoBuffering
>               inputLoop
>     
>     inputLoop :: IO ()
>     inputLoop = do i <- getContents
>                    mapM_ putChar $ takeWhile ((/= 27) . ord) i
>     
> 
> Because of the hSetBuffering stdin NoBuffering line it should not be necessary to press the enter key between keystrokes. This program works correctly in WinHugs (sep 2006 version). However, GHC 6.8.2 does not repeat the characters until the enter key is pressed. The problem was reproduced with all GHC executables (ghci, ghc, runghc, runhaskell), using both cmd.exe and command.com on Windows XP Professional...
