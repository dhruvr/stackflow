
# What is the benefit of purely functional data structure?

## Question
        
There are large number of texts on data structures, and libraries of data structures code. I understand that purely functional data structure is easier to reason about. However I have trouble to understand the real world advantage of using purely functional data structure in pragmatic code (using functional programming language or not) over the imperative counterpart. Can somebody provide some real world cases where purely functional data structure has advantage and why?

Examples along the line like I use **data\_structure\_name** in **programming_language** to do **application** because it can do **certain_thing**.

Thanks.

PS: What I mean by purely functional data structure is not the same as persistent data structure. Persistent data structure is a data structure that doesn't change?? On other hand purely functional data structure is a data structure that operates purely.

## Answer
        
Purely functional (aka persistent or immutable) data structures give you several advantages:

*   you never have to lock them, which extremely improves **concurrency**.
*   they can share structure, which **reduces memory usage**. For example, consider list \[1, 2, 3, 4\] in Haskell and some imperative language like Java. To produce new list in Haskell you only have to create new `cons` (pair of value and reference-to-next-element) and connect it to the previous list. In Java you have to create completely new list not to damage the previous one.
*   you can make persistent data structures **[lazy](http://en.wikipedia.org/wiki/Lazy_evaluation)**.
*   also, if you use functional style, you can **avoid thinking of time and sequence of operations**, and so, make your programs more [declarative](http://en.wikipedia.org/wiki/Declarative_programming).
*   fact, that the data structure is immutable, allows you to make some more assumptions and so **expand capabilities of language**. For example, [Clojure](http://clojure.org/) uses the fact of immutability to correctly provide implementations of hashCode() method on each object, so any object may be used as a key in a map.
*   with immutable data and functional style you can also freely use **[memoization](http://en.wikipedia.org/wiki/Memoization)**.

There's much more advantages, in general, it is another way of modeling the real world. [This](http://mitpress.mit.edu/sicp/full-text/book/book-Z-H-24.html#%_sec_3.5.5) and some other chapters from SICP will give you more accurate view of programming with immutable structures, its advantages and disadvantages.
