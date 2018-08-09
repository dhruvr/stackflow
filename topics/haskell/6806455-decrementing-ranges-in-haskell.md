
# Decrementing ranges in Haskell

## Question
        
I am very new to Haskell. Could someone please explain why defining a list like this returns an null list

    ghci>  let myList = [10..1]
    ghci>  myList
    []
    

However this works correctly.

    ghci>  let myList = [10, 9..1]
    ghci>  myList
    [10, 9, 8, 7, 6, 5, 4, 3, 2, 1]

## Answer
        
Basically, because `[10..1]` is translated to `enumFromTo 10 1` which itself has the semantics to create a list by taking all elements less-than `1` which result from counting upward (with step-size `+1`) from (including) `10`.

Whereas `[10, 9..1]` is translated to `enumFromToThen 10 9 1` which explicitly states the counting step-size as `9-10`, i.e. `-1` (which is hard-coded to `+1` for `enumFromTo`)

A more accurate specification can be found in the Haskell Report (6.3.4 The Enum Class):

    enumFrom       :: a -> [a]            -- [n..]
    enumFromThen   :: a -> a -> [a]       -- [n,n'..]
    enumFromTo     :: a -> a -> [a]       -- [n..m]
    enumFromThenTo :: a -> a -> a -> [a]  -- [n,n'..m]
    

> For the types `Int` and `Integer`, the enumeration functions have the following meaning:
> 
> *   The sequence `enumFrom e1` is the list `[e1,e1+1,e1+2,...]`.
>     
> *   The sequence `enumFromThen e1 e2` is the list `[e1,e1+i,e1+2i,...]`, where the increment, i, is e2-e1. The increment may be zero or negative. If the increment is zero, all the list elements are the same.
>     
> *   The sequence `enumFromTo e1 e3` is the list `[e1,e1+1,e1+2,...e3]`. The list is empty if `e1 > e3`.
>     
> *   The sequence `enumFromThenTo e1 e2 e3` is the list `[e1,e1+i,e1+2i,...e3]`, where the increment, `i`, is `e2-e1`. If the increment is positive or zero, the list terminates when the next element would be greater than `e3`; the list is empty if `e1 > e3`. If the increment is negative, the list terminates when the next element would be less than `e3`; the list is empty if `e1 < e3`.
>
