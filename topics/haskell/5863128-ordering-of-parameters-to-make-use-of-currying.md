
# Ordering of parameters to make use of currying

## Question
        
I have twice recently refactored code in order to change the order of parameters because there was too much code where hacks like `flip` or `\x -> foo bar x 42` were happening.

When designing a function signature what principles will help me to make the best use of currying?

## Answer
        
For languages that support currying and partial-application easily, there is one compelling series of arguments, originally from Chris Okasaki:

*   **Put the data structure as the last argument**

Why? You can then [compose operations on the data](http://www.haskell.org/pipermail/libraries/2005-August/004297.html) nicely. E.g. `insert 1 $ insert 2 $ insert 3 $ s`. This also helps for [functions on state](http://www.haskell.org/pipermail/libraries/2005-August/004323.html).

Standard libraries such as "containers" [follow this convention](http://hackage.haskell.org/packages/archive/containers/0.4.0.0/doc/html/Data-Map.html#g:5).

Alternate arguments are sometimes given to put the data structure first, so it can be closed over, yielding functions on a static structure (e.g. lookup) that are a bit more concise. However, the broad consensus seems to be that this is less of a win, especially since it pushes you towards heavily parenthesized code.

*   **Put the most varying argument last**

For recursive functions, it is common to put the argument that varies the most (e.g. an accumulator) as the last argument, while the argument that varies the least (e.g. a function argument) at the start. This composes well with the data structure last style.

* * *

A summary of the Okasaki view is given in [his Edison library](http://hackage.haskell.org/packages/archive/EdisonAPI/1.2.1/doc/html/Data-Edison.html) (again, another data structure library):

*   **Partial application**: arguments more likely to be static usually appear before other arguments in order to facilitate partial application.
*   **Collection appears last**: in all cases where an operation queries a single collection or modifies an existing collection, the collection argument will appear last. This is something of a de facto standard for Haskell datastructure libraries and lends a degree of consistency to the API.
*   **Most usual order**: where an operation represents a well-known mathematical function on more than one datastructure, the arguments are chosen to match the most usual argument order for the function.
