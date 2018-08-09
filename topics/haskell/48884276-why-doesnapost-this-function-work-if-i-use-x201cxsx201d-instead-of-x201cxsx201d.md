
# Why doesn&apos;t this function work if I use &#x201C;[xs]&#x201D; instead of &#x201C;xs&#x201D;?

## Question
        
    split :: [a] -> Int -> ([a], [a])
    split [xs] n = 
        (take n [xs], drop n [xs])
    

The same code works if I give the variable as `xs` instead of `[xs]`, signatures are same in both cases. Using `[xs]` gives the error that pattern is non-exhaustive. I understand it's telling that the input I gave is not covered by my code, but not clear what is happening under the hood.

Test input: `[1,2,3] 2`.

## Answer
        
Somehow a lot of people think that `[xs]` as pattern means that you _unify_ a list with `xs`. But this is incorrect, since the function signature (either derived implicitly, or stated explicitly) already will prevent you to write code where you call the function with a non-list item.

A list has _two_ constructors:

*   the empty list `[]`; and
*   the "cons" `(h : t)` with `h` the _head_ (first element), and `t` the _tail_ (a list with the remaining elements).

Haskell however introduces some syntactical sugar as well. For example `[1]` is short for `(1:[])`, and `[1, 4, 2]` for `(1:(4:(2:[])))`.

So that means that if you write `[xs]`, behind the curtains you defined a pattern `(xs: [])` which thus means you match all lists with _exactly_ one element, and that single _element_ (not the entire list) is then `xs`.

Anyway, the solution is to use:

    split xs n = (take n xs, drop n xs)

Since both [**`take :: Int -> [a] -> [a]`**](http://hackage.haskell.org/package/base-4.10.1.0/docs/Prelude.html#v:take) and [**`drop :: Int -> [a] -> [a]`**](http://hackage.haskell.org/package/base-4.10.1.0/docs/Prelude.html#v:drop) have in the signature that `xs` is supposed to be a list, Haskell will derive automatically that `n` is supposed to be an `Int`, and `xs` an `[a]`.

Note that you can use [**`splitAt :: Int -> [a] -> ([a], [a])`**](http://hackage.haskell.org/package/base-4.10.1.0/docs/Prelude.html#v:splitAt) as well. We can make the signature equivalent to the one you target with:

    split = flip splitAt
