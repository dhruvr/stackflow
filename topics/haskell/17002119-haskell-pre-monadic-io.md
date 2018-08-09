
# Haskell pre-monadic I/O

## Question
        
I wonder how I/O were done in Haskell in the days when IO monad was still not invented. Anyone knows an example.

Edit: Can I/O be done without the IO Monad in modern Haskell? I'd prefer an example that works with modern GHC.

## Answer
        
Before the IO monad was introduced, `main` was a function of type `[Response] -> [Request]`. A `Request` would represent an I/O action like writing to a channel or a file, or reading input, or reading environment variables etc.. A `Response` would be the result of such an action. For example if you performed a `ReadChan` or `ReadFile` request, the corresponding `Response` would be `Str str` where `str` would be a `String` containing the read input. When performing an `AppendChan`, `AppendFile` or `WriteFile` request, the response would simply be `Success`. (Assuming, in all cases, that the given action was actually successful, of course).

So a Haskell program would work by building up a list of `Request` values and reading the corresponding responses from the list given to `main`. For example a program to read a number from the user might look like this (leaving out any error handling for simplicity's sake):

    main :: [Response] -> [Request]
    main responses =
      [
        AppendChan "stdout" "Please enter a Number\n",
        ReadChan "stdin",
        AppendChan "stdout" . show $ enteredNumber * 2
      ]
      where (Str input) = responses !! 1
            firstLine = head . lines $ input
            enteredNumber = read firstLine 
    

As Stephen Tetley already pointed out in a comment, a detailed specification of this model is given in chapter 7 of the [1.2 Haskell Report](http://haskell.cs.yale.edu/wp-content/uploads/2011/01/haskell-report-1.2.pdf).

* * *

> Can I/O be done without the IO Monad in modern Haskell?

No. Haskell no longer supports the `Response`/`Request` way of doing IO directly and the type of `main` is now `IO ()`, so you can't write a Haskell program that doesn't involve `IO` and even if you could, you'd still have no alternative way of doing any I/O.

What you can do, however, is to write a function that takes an old-style main function and turns it into an IO action. You could then write everything using the old style and then only use IO in `main` where you'd simply invoke the conversion function on your real main function. Doing so would almost certainly be more cumbersome than using the `IO` monad (and would confuse the hell out of any modern Haskeller reading your code), so I definitely would not recommend it. However it _is_ possible. Such a conversion function could look like this:

    import System.IO.Unsafe
    
    -- Since the Request and Response types no longer exist, we have to redefine
    -- them here ourselves. To support more I/O operations, we'd need to expand
    -- these types
    
    data Request =
        ReadChan String
      | AppendChan String String
    
    data Response =
        Success
      | Str String
      deriving Show
    
    -- Execute a request using the IO monad and return the corresponding Response.
    executeRequest :: Request -> IO Response
    executeRequest (AppendChan "stdout" message) = do
      putStr message
      return Success
    executeRequest (AppendChan chan _) =
      error ("Output channel " ++ chan ++ " not supported")
    executeRequest (ReadChan "stdin") = do
      input <- getContents
      return $ Str input
    executeRequest (ReadChan chan) =
      error ("Input channel " ++ chan ++ " not supported")
    
    -- Take an old style main function and turn it into an IO action
    executeOldStyleMain :: ([Response] -> [Request]) -> IO ()
    executeOldStyleMain oldStyleMain = do
      -- I'm really sorry for this.
      -- I don't think it is possible to write this function without unsafePerformIO
      let responses = map (unsafePerformIO . executeRequest) . oldStyleMain $ responses
      -- Make sure that all responses are evaluated (so that the I/O actually takes
      -- place) and then return ()
      foldr seq (return ()) responses
    

You could then use this function like this:

    -- In an old-style Haskell application to double a number, this would be the
    -- main function
    doubleUserInput :: [Response] -> [Request]
    doubleUserInput responses =
      [
        AppendChan "stdout" "Please enter a Number\n",
        ReadChan "stdin",
        AppendChan "stdout" . show $ enteredNumber * 2
      ]
      where (Str input) = responses !! 1
            firstLine = head . lines $ input
            enteredNumber = read firstLine 
    
    main :: IO ()
    main = executeOldStyleMain doubleUserInput
