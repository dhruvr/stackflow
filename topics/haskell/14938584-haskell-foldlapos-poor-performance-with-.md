
# Haskell foldl&apos; poor performance with (++)

## Question
        
I have this code:

    import Data.List
    
    newList_bad  lst = foldl' (\acc x -> acc ++ [x*2]) [] lst
    newList_good lst = foldl' (\acc x -> x*2 : acc) [] lst
    

These functions return lists with each element multiplied by 2:

    *Main> newList_bad [1..10]
    [2,4,6,8,10,12,14,16,18,20]
    *Main> newList_good [1..10]
    [20,18,16,14,12,10,8,6,4,2]
    

In ghci:

    *Main> sum $ newList_bad [1..15000]
    225015000
    (5.24 secs, 4767099960 bytes)
    *Main> sum $ newList_good [1..15000]
    225015000
    (0.03 secs, 3190716 bytes)
    

Why `newList_bad` function works 200 times slower than `newList_good`? I understand that it's not a good solution for that task. But why this innocent code works so slow?

What is this "4767099960 bytes"?? For that simple an operation Haskell used 4 GiB??

After compilation:

    C:\1>ghc -O --make test.hs
    C:\1>test.exe
    225015000
    Time for sum (newList_bad [1..15000]) is 4.445889s
    225015000
    Time for sum (newList_good [1..15000]) is 0.0025005s

## Answer
        
Classic list behavior.

Recall:

    (:)  -- O(1) complexity
    (++) -- O(n) complexity
    

So you are creating an O(n^2) algo, instead of an O(n) one.

For this common case of appending to lists incrementally, try using a [dlist](http://hackage.haskell.org/packages/archive/dlist/0.3/doc/html/Data-DList.html), or just reverse at the end.
