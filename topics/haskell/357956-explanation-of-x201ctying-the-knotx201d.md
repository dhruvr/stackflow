
# Explanation of &#x201C;tying the knot&#x201D;

## Question
        
In reading Haskell-related stuff I sometimes come across the expression “tying the knot”, I think I understand _what_ it does, but not _how_.

So, are there any good, basic, and simple to understand explanations of this concept?

## Answer
        
Tying the knot is a solution to the problem of circular data structures. In imperative languages you construct a circular structure by first creating a non-circular structure, and then going back and fixing up the pointers to add the circularity.

Say you wanted a two-element circular list with the elements "0" and "1". It would seem impossible to construct because if you create the "1" node and then create the "0" node to point at it, you cannot then go back and fix up the "1" node to point back at the "0" node. So you have a chicken-and-egg situation where both nodes need to exist before either can be created.

Here is how you do it in Haskell. Consider the following value:

    alternates = x where
       x = 0 : y
       y = 1 : x
    

In a non-lazy language this will be an infinite loop because of the unterminated recursion. But in Haskell lazy evaluation does the Right Thing: it generates a two-element circular list.

To see how it works in practice, think about what happens at run-time. The usual "thunk" implementation of lazy evaluation represents an unevaluated expression as a data structure containing a function pointer plus the arguments to be passed to the function. When this is evaluated the thunk is replaced by the actual value so that future references don't have to call the function again.

When you take the first element of the list 'x' is evaluated down to a value (0, &y), where the "&y" bit is a pointer to the value of 'y'. Since 'y' has not been evaluated this is currently a thunk. When you take the second element of the list the computer follows the link from x to this thunk and evaluates it. It evaluates to (1, &x), or in other words a pointer back to the original 'x' value. So you now have a circular list sitting in memory. The programmer doesn't need to fix up the back-pointers because the lazy evaluation mechanism does it for you.
