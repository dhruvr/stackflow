
# What is the difference between Int and Integer?

## Question
        
In Haskell, what is the difference between an `Int` and an `Integer`? Where is the answer documented?

## Answer
        
> "Integer" is an arbitrary precision type: it will hold any number no matter how big, up to the limit of your machine's memory…. This means you never have arithmetic overflows. On the other hand it also means your arithmetic is relatively slow. Lisp users may recognise the "bignum" type here.
> 
> "Int" is the more common 32 or 64 bit integer. Implementations vary, although it is guaranteed to be at least 30 bits.

Source: [The Haskell Wikibook](http://en.wikibooks.org/wiki/Haskell/A_Miscellany_of_Types). Also, you may find the [Numbers](http://www.haskell.org/tutorial/numbers.html) section of _A Gentle Introduction to Haskell_ useful.
