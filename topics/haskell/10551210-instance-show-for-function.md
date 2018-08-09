
# Instance Show for function

## Question
        
I'm currently trying to write a little Show instance for primitive arithmetic functions.

Goal ist to make a list of functions showable.

The very simple function for show looks like that:

      showOp :: (Int -> Int -> Int) -> String
      showOp op
        | op 3 3 == 6 = "plus"
        | op 3 3 == 0 = "minus"
        | op 3 3 == 9 = "times"
        | op 3 3 == 1 = "divide"
        | otherwise = "undefined"
    

But I can't get an instance of Show for (Int -> Int -> Int). I tried it like that:

        instance Show (Int -> Int -> Int) where
        show op = show "asdf"
    

But it doesn't work. WinHugs just returns the Error

        Syntax error in instance head (variable expected)
    

Is it even possible to define Show for functions? If it is, how could I tackle that problem?

## Answer
        
Don't use WinHugs. Use GHC.

In fact, in recent Haskell Platform versions there is already an instance of functions for Show.

    Prelude Text.Show.Functions> show (+1)
    "<function>"
    Prelude Text.Show.Functions> show (\x -> x ++ "foo")
    "<function>"
    

Now, in your case, however, you need `-XFlexibleInstances` on, since your instance isn't of the form `(Constructor a1 .. an)` where a1 .. an are distinct type variables.

Turn it on with `{-# LANGUAGE FlexibleInstances #-}`
