
# What is the difference between $ (dollar) and $! (dollar exclamation point)

## Question
        
Can anybody explain the difference in Haskell between the operators `($)` and `($!)` (dollar sign vs dollar sign exclamation point)?

I haven't seen the use of `$!` anywhere so far, but while browsing through the Haskell [reference](http://www.zvon.org), I noticed its existence and that it has the exact same definition as `$`. When trying some simple statements in a Haskell interpreter ([GHCi](http://www.haskell.org/haskellwiki/GHC/GHCi)), I couldn't find any difference, nor could I find any reference to the operator in the top listed tutorials when searching for `haskell tutorial`.

So, just out of curiosity, what is the difference, if at all?

## Answer
        
`($!)` is strict function application. That is, it evaluates the argument before evaluating the function.

This is contrary to normal lazy function application in Haskell, e.g. `f x` or `f $ x`, which first start to evaluate the function `f`, and only compute the argument `x` if it is needed.

For example `succ (1 + 2)` will delay the addition `1 + 2` by creating a thunk, and start to evaluate `succ` first. Only if the argument to succ is needed, will `1 + 2` be evaluated.

However, if you know for sure that the argument to a function will always be needed, you can use `($!)`, which will first evaluate the argument to weak head normal form, and then enter the function. This way, you don't create a whole big pile of thunks and this can be more efficient. In this example, `succ $! 1 + 2` would first compute `3` and then enter the function `succ`.

Note that it is not always safe to just replace normal function application with strict function application. For example:

    ghci> const 1 (error "noo!")
    1
    ghci> const 1 $! (error "noo!")
    *** Exception: noo!
