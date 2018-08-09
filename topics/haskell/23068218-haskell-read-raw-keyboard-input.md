
# Haskell read raw keyboard input

## Question
        
I'm writing a terminal-mode program in Haskell. How would I go about reading raw keypress information?

In particular, there seems to be something providing line-editing facilities on top of Haskell. If I do `getLine`, I seem to be able to use the up-arrow to get previous lines, edit the text, and only when I press Enter does the text become visible to the Haskell application itself.

What I'm after is the ability to read individual keypresses, so I can implement the line-editing stuff myself.

* * *

Perhaps my question was unclear. Basically I want to build something like Vi or Emacs (or [Yi](http://www.haskell.org/haskellwiki/Yi)). I already know there are terminal bindings that will let me do fancy console-mode printing, so the output side shouldn't be an issue. I'm just looking for a way to get at raw keypress input, so I can do things like (for example) add K to the current line of text when the user presses the letter K, or save the file to disk when the user presses Ctrl+S.

## Answer
        
Sounds like you want readline support. There are a couple of packages to do this, but [haskeline](http://hackage.haskell.org/package/haskeline) is probably the easiest to use with the most supported platforms.

    import Control.Monad.Trans
    import System.Console.Haskeline
    
    type Repl a = InputT IO a
    
    process :: String -> IO ()
    process = putStrLn
    
    repl :: Repl ()
    repl = do
      minput <- getInputLine "> "
      case minput of
        Nothing -> outputStrLn "Goodbye."
        Just input -> (liftIO $ process input) >> repl
    
    main :: IO ()
    main = runInputT defaultSettings repl
