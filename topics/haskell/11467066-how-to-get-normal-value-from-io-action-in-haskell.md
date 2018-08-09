
# How to get normal value from IO action in Haskell

## Question
        
I have the following function:

    get :: Chars -> IO Chars
    get cs = do
        char <- getChar
        let (dats, idx) = (curData cs, curIndex cs)
        let (x,y:xs) = splitAt idx dats
        let replacement = x ++ (ord char) : xs
        return $ Chars replacement idx
    

and I'm wanting to get a `Chars` out of it, _not_ an IO action. I have _no_ idea how to do this, or if it is even possible.

Chars is basically just a container with an `[Int]` called curData and an `Int` called curIndex. The specifics aren't that important, I just want to know if there's a way for this function to return a `Chars` instead of an `IO Chars`.

If not, how do I pass this as an argument to a function that takes a `Chars`? I'm kind of new to Haskell IO, but I don't think I want all of my functions that take `Chars` as arguments to instead have to take `IO Chars` as arguments, and then extract and repackage them. It seems unnecessary.

Thanks!

## Answer
        
You can't, because that would violate [referential transparency](http://en.wikipedia.org/wiki/Referential_transparency_%28computer_science%29).

IO in Haskell is made this way exactly to distinguish between actions whose result and effects may vary depending on the interaction with the environment/user and pure functions whose results are not going to change when you call them with the same input parameters.

In order to pass the result to a pure function taking a `Chars` in input you have to call your IO action into another IO action, bind the result with the `<-` operator to a variable and pass it to your pure function. Pseudocode example:

    myPureFunction :: Chars -> ...
    
    otherAction :: Chars -> IO ()
    otherAction cs = do
      myChars <- get cs
      let pureResult = myPureFunction myChars
      ...
    

If you're new to IO in haskell, you may wish to have a look at the Input and Output chapters in [Learn You a Haskell for a Great Good!](http://learnyouahaskell.com/input-and-output) and [Real World Haskell](http://book.realworldhaskell.org/read/io.html).

There is actually a way to simply get a pure value out of an IO action, but in your case you shouldn't do it, as you're interacting with the environment: the unsafe way is ok only when you can guarantee you're not violating referential transparency.
