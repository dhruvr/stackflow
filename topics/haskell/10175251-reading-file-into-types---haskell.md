
# Reading file into types - Haskell

## Question
        
Right now I have two types:

    type Rating = (String, Int)
    
    type Film = (String, String, Int, [Rating])
    

I have a file that has this data in it:

    "Blade Runner"
    "Ridley Scott"
    1982
    ("Amy",5), ("Bill",8), ("Ian",7), ("Kevin",9), ("Emma",4), ("Sam",7), ("Megan",4)
    
    "The Fly"
    "David Cronenberg"
    1986
    ("Megan",4), ("Fred",7), ("Chris",5), ("Ian",0), ("Amy",6)
    

How can I look through then file storing all of the entries into something like FilmDatabase = \[Film\] ?

## Answer
        
Haskell provides a unique way of sketching out your approach. Begin with what you know

    module Main where
    
    type Rating = (String, Int)
    type Film = (String, String, Int, [Rating])
    
    main :: IO ()
    main = do
      films <- readFilms "ratings.dat"
      print films
    

Attempting to load this program into ghci will produce

films.hs:8:12: Not in scope: `readFilms'

It needs to know what `readFilms` is, so add just enough code to keep moving.

    readFilms = undefined
    

It is a function that should do something related to `Film` data. Reload this code (with the `:reload` command or `:r` for short) to get

films.hs:9:3:
    Ambiguous type variable `a0' in the constraint:
      (Show a0) arising from the use of `print'
    ...

The type of `print` is

Prelude> :t print
print :: Show a => a -> IO ()

In other words, `print` takes a single argument that, informally, knows how to show itself (that is, convert its contents to a string) and creates an I/O action that when executed outputs that string. It’s more-or-less how you expect `print` to work:

Prelude> print 3
3
Prelude> print "hi"
"hi"

We know that we want to `print` the `Film` data from the file, but, although good, ghc can’t read our minds. But after adding a type hint

    readFilms :: FilePath -> Film
    readFilms = undefined
    

we get a new error.

films.hs:8:12:
    Couldn't match expected type `IO t0'
                with actual type `(String, String, Int, \[Rating\])'
    Expected type: IO t0
      Actual type: Film
    In the return type of a call of `readFilms'
    In a stmt of a 'do' expression: films <- readFilms "ratings.dat"

The error tells you that the compiler is confused about your story. You said `readFilms` should give it back a `Film`, but the way you called it in `main`, the computer should have to first perform some I/O and _then_ give back `Film` data.

In Haskell, this is the difference between a _pure_ string, say `"JamieB"`, and a side effect, say reading your input from the keyboard after prompting you to input your Stack Overflow username.

So now we know we can sketch `readFilms` as

    readFilms :: FilePath -> IO Film
    readFilms = undefined
    

and the code compiles! (But we can’t yet run it.)

To dig down another layer, pretend that the name of a single movie is the only data in `ratings.dat` and put placeholders everywhere else to keep the typechecker happy.

    readFilms :: FilePath -> IO Film
    readFilms path = do
      alldata <- readFile path
      return (alldata, "", 0, [])
    

This version compiles, and you can even run it by entering `main` at the ghci prompt.

In [dave4420’s answer](https://stackoverflow.com/a/10175462/123109) are great hints about other functions to use. Think of the method above as putting together a jigsaw puzzle where the individual pieces are functions. For your program to be correct, all the types must fit together. You can make progress toward your final working program by taking little babysteps as above, and the typechecker will let you know if you have a mistake in your sketch.

Things to figure out:

*   How do you convert the whole blob of input to individual lines?
*   How do you figure out whether the line your program is examining is a title, a director, and so on?
*   How do you convert the year in your file (a `String`) to an `Int` to cooperate with your definition of `Film`?
*   How do you skip blank or empty lines?
*   How do you make `readFilms` accumulate and return a list of `Film` data?
