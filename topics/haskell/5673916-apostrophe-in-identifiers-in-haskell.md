
# Apostrophe in identifiers in Haskell

## Question
        
I found this code snipped on the internet:

    digits 0 = [0]
    digits n = digits' n []
      where digits' 0 ds = ds
            digits' n ds = let (q,r) = quotRem n 10
                           in digits' q (r:ds)
    
    sumOfDigits = sum . digits
    

Can someone quickly explain what the " ' " sign ( `digits n = digits' n []` ) after the recursive function call is for? I've seen some other code examples in Haskell (tutorials), but im not understandig this one. A quick explanation is appreciated.

## Answer
        
The apostrophe is just part of the name. It is a naming convention (idiom) adopted in Haskell.

The convention in Haskell is that, [like in math](http://en.wikipedia.org/wiki/Prime_%28symbol%29#Use_in_mathematics.2C_statistics.2C_and_science), the apostrophe on a variable name represents a variable that is somehow related, or similar, to a prior variable.

An example:

    let x  = 1
        x' = x * 2
    in x'
    

`x'` is related to `x`, and we indicate that with the apostrophe.

* * *

You can run this in GHCi, by the way,

    Prelude> :{ 
    Prelude| let x  = 1
    Prelude|     x' = x * 2
    Prelude| in x'
    Prelude| :}
    2
