
# Getting a list of all possible data type values in Haskell

## Question
        
If I have a data type say:

    data Color = Red | Yellow | Green
    

Is there a way I can turn this into a list of type \[Color\] getting all possible values? \[Red, Yellow, Green\]

Perhaps this is a complete anti pattern?

## Answer
        
    data Color = Red
               | Yellow
               | Green
               deriving Enum
    
    allColors = [Red ..]
