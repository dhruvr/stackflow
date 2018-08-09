
# Haskell &#x201C;where&#x201D; indentation: why must it be indented past identifier?

## Question
        
This code:

    import Data.Char (digitToInt)
    
    myInt :: String -> Int
    myInt [] = error "bad input: empty string"
    myInt (x:xs)
      | x == '-'  = -1 * myInt xs
      | otherwise = foldl convert 0 (x:xs)
      where convert acc x
            | x `elem` ['0'..'9'] = 10 * acc + digitToInt x
            | otherwise           = error ("bad input: not an int - " ++ [x])
    

Fails:

    Prelude> :l safeListFs.hs
    [1 of 1] Compiling Main             ( safeListFs.hs, interpreted )
    
    safeListFs.hs:9:8: parse error (possibly incorrect indentation)
    Failed, modules loaded: none.
    

But this version:

    import Data.Char (digitToInt)
    
    myInt :: String -> Int
    myInt [] = error "bad input: empty string"
    myInt (x:xs)
      | x == '-'  = -1 * myInt xs
      | otherwise = foldl convert 0 (x:xs)
      where convert acc x
              | x `elem` ['0'..'9'] = 10 * acc + digitToInt x
              | otherwise           = error ("bad input: not an int - " ++ [x])
    

is ok:

    Prelude> :l safeListFs.hs
    [1 of 1] Compiling Main             ( safeListFs.hs, interpreted )
    Ok, modules loaded: Main.
    

I can't figure out why those two last indents matter.

## Answer
        
Basically, Haskell notes the column where the first non-space character after `where` appears (in this case, the `c` of `convert`) and treats following lines beginning in that column as new definitions inside the `where`.

A line that continues the definition of the previous line (such as your `|` guards) must be indented to the right of _the first non-space character_ (`c` in your code).

A line indented to the left of `c` would be outside the `where` (for example, the start of your next top-level function).

It's the column of the first character following `where` that is crucial, even if it's on a new line:

      where
        convert acc x
          | ...
        anotherFunction x y
    
        ^
