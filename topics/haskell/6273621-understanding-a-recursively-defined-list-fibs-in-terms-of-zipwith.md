
# Understanding a recursively defined list (fibs in terms of zipWith)

## Question
        
I'm learning Haskell, and came across the following code:

    fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
    

which I'm having a bit of trouble parsing, in terms of how it works. It's very neat, I understand that nothing more is needed, but I'd like to understand how Haskell manages to "fill in" fibs when I write:

    take 50 fibs
    

Any help?

Thanks!

## Answer
        
I'll give a bit of an explanation of how it works internally. First, you must realise that Haskell uses a thing called a [thunk](http://en.wikipedia.org/wiki/Thunk_%28functional_programming%29) for its values. A thunk is basically a value that has not yet been computed yet -- think of it as a function of 0 arguments. Whenever Haskell wants to, it can evaluate (or partly-evaluate) the thunk, turning it in to a real value. If it only _partly_ evaluates a thunk, then the resulting value will have more thunks in it.

For example, consider the expression:

    (2 + 3, 4)
    

In an ordinary language, this value would be stored in memory as `(5, 4)`, but in Haskell, it is stored as `(<thunk 2 + 3>, 4)`. If you ask for the second element of that tuple, it will tell you "4", without ever adding 2 and 3 together. Only if you ask for the first element of that tuple will it evaluate the thunk, and realise that it is 5.

With fibs, it's a bit more complicated, because it's recursive, but we can use the same idea. Because `fibs` takes no arguments, Haskell will permanently store any list elements that have been discovered -- that is important.

    fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
    

It helps to visualise Haskell's current knowledge of three expressions: `fibs`, `tail fibs` and `zipWith (+) fibs (tail fibs)`. We shall assume Haskell starts out knowing the following:

    fibs                         = 0 : 1 : <thunk1>
    tail fibs                    = 1 : <thunk1>
    zipWith (+) fibs (tail fibs) = <thunk1>
    

Note that the 2nd row is just the first one shifted left, and the 3rd row is the first two rows summed.

Ask for `take 2 fibs` and you'll get `[0, 1]`. Haskell doesn't need to further evaluate the above to find this out.

Ask for `take 3 fibs` and Haskell will get the 0 and 1, and then realise that it needs to _partly evaluate_ the thunk. In order to fully evaluate `zipWith (+) fibs (tail fibs)`, it needs to sum the first two rows -- it can't fully do that, but it can _begin_ to sum the first two rows:

    fibs                         = 0 : 1 : 1: <thunk2>
    tail fibs                    = 1 : 1 : <thunk2>
    zipWith (+) fibs (tail fibs) = 1 : <thunk2>
    

Note that I filled in the "1" in the 3rd row, and it automatically appeared in the first and second rows as well, since all three rows are sharing the same thunk (think of it like a pointer that got written to). And because it didn't finish evaluating, it created a new thunk containing the _rest_ of the list, should that ever be needed.

It isn't needed, though, because `take 3 fibs` is done: `[0, 1, 1]`. But now, say you ask for `take 50 fibs`; Haskell already remembers the 0, 1 and 1. But it needs to keep going. So it continues summing the first two rows:

    fibs                         = 0 : 1 : 1 : 2 : <thunk3>
    tail fibs                    = 1 : 1 : 2 : <thunk3>
    zipWith (+) fibs (tail fibs) = 1 : 2 : <thunk3>
    

...

    fibs                         = 0 : 1 : 1 : 2 : 3 : <thunk4>
    tail fibs                    = 1 : 1 : 2 : 3 : <thunk4>
    zipWith (+) fibs (tail fibs) = 1 : 2 : 3 : <thunk4>
    

And so on, until it has filled in 48 columns of the 3rd row, and thus has worked out the first 50 numbers. Haskell evaluates just as much as it needs, and leaves the infinite "rest" of the sequence as a thunk object in case it ever needs any more.

Note that if you subsequently ask for `take 25 fibs`, Haskell will not evaluate it again -- it will just take the first 25 numbers from the list it has already calculated.

**Edit**: Added a unique number to each thunk to avoid confusion.
