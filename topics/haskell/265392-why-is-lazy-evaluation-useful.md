
# Why is lazy evaluation useful?

## Question
        
I have long been wondering why lazy evaluation is useful. I have yet to have anyone explain to me in a way that makes sense; mostly it ends up boiling down to "trust me".

Note: I do not mean memoization.

## Answer
        
Mostly because it can be more efficient -- values don't need to be computed if they're not going to be used. For example, I may pass three values into a function, but depending on the sequence of conditional expressions, only a subset may actually be used. In a language like C, all three values would be computed anyway; but in Haskell, only the necessary values are computed.

It also allows for cool stuff like infinite lists. I can't have an infinite list in a language like C, but in Haskell, that's no problem. Infinite lists are used fairly often in certain areas of mathematics, so it can be useful to have the ability to manipulate them.
