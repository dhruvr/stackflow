
# How to define a function in ghci across multiple lines?

## Question
      
I'm trying to define any simple function that spans multiple lines in ghci, take the following as an example:

    let abs n | n >= 0 = n
              | otherwise = -n
    

So far I've tried pressing Enter after the first line:

    Prelude> let abs n | n >= 0 = n
    Prelude>           | otherwise = -n
    <interactive>:1:0: parse error on input `|'
    

I've also attempted to use the `:{` and `:}` commands but I don't get far:

    Prelude> :{
    unknown command ':{'
    use :? for help.
    

I'm using GHC Interactive version 6.6 for Haskell 98 on Linux, what am I missing?
## Answer
      
for guards (like your example), you can just put them all on one line and it works (guards do not care about spacing)

    let abs n | n >= 0 = n | otherwise = -n
    

if you wanted to write your function with multiple definitions that pattern match on the arguments, like this:

    fact 0 = 1
    fact n = n * fact (n-1)
    

then you would use braces with semicolons separating the definitions

    let { fact 0 = 1 ; fact n = n * fact (n-1) }
    