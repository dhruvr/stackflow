
# GHCi &#x201C;let&#x201D; &#x2014; what does it do?

## Question
        
I'd appreciate is someone could point to docs on what "let" does in GHCi, or failing that, explain it convincingly :-).

So far as I can tell, "let" (without "in") is not part of the Haskell language per se, and on the other hand, it doesn't appear to be a GHCI command either, as it's not prefixed by colon.

## Answer
        
While programming in GHCi, you're like programming in the IO monad with `do` syntax, so for example you can directly execute an `IO` action, or use monadic bind syntax like `r <- someIOFun`.

`let` is also a part of `do` so you can also use this. I think it's being desugared into `let .. in <rest of the computation>`, so for example when you do this:

    ghci> let a = 1
    ghci> someFun
    ghci> someFun2
    

It's like:

    let a = 1 in
    do someFun
       someFun2
