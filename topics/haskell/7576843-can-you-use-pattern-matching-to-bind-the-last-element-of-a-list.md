
# Can you use pattern matching to bind the last element of a list?

## Question
        
Since there is a way to bind the head and tail of a list via pattern matching, I'm wondering if you can use pattern matching to bind the last element of a list?

## Answer
        
Yes, you can, using the `ViewPatterns` extension.

    Prelude> :set -XViewPatterns
    Prelude> let f (last -> x) = x*2
    Prelude> f [1, 2, 3]
    6
    

Note that this pattern will always succeed, though, so you'll probably want to add a pattern for the case where the list is empty, else `last` will throw an exception.

    Prelude> f []
    *** Exception: Prelude.last: empty list
    

Also note that this is just syntactic sugar. Unlike normal pattern matching, this is _O(n)_, since you're still accessing the last element of a singly-linked list. If you need more efficient access, consider using a different data structure such as [`Data.Sequence`](http://www.haskell.org/ghc/docs/latest/html/libraries/containers/Data-Sequence.html), which offers _O(1)_ access to both ends.
