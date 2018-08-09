
# Why do we need monads?

## Question
        
In my humble opinion the answers to the famous question ["What is a monad?"](https://stackoverflow.com/questions/44965/what-is-a-monad), especially the most voted ones, try to explain what is a monad without clearly explaining _why monads are really necessary_. Can they be explained as the solution to a problem?

## Answer
        
Why do we need monads?
----------------------

1.  We want to program **only using functions**. ("functional programming (FP)" after all).
2.  Then, we have a first big problem. This is a program:
    
    `f(x) = 2 * x`
    
    `g(x,y) = x / y`
    
    How can we say **what is to be executed first**? How can we form an ordered sequence of functions (i.e. **a program**) _using no more than functions_?
    
    Solution: **compose functions**. If you want first `g` and then `f`, just write `f(g(x,y))`. This way, "the program" is a function as well: `main = f(g(x,y))`. OK, but ...
    
3.  More problems: some functions **might fail** (i.e. `g(2,0)`, divide by 0). We have **no "exceptions"** in FP (an exception is not a function). How do we solve it?
    
    Solution: Let's **allow functions to return two kind of things**: instead of having `g : Real,Real -> Real` (function from two reals into a real), let's allow `g : Real,Real -> Real | Nothing` (function from two reals into (real or nothing)).
    
4.  But functions should (to be simpler) return only **one thing**.
    
    Solution: let's create a new type of data to be returned, a "**boxing type**" that encloses maybe a real or be simply nothing. Hence, we can have `g : Real,Real -> Maybe Real`. OK, but ...
    
5.  What happens now to `f(g(x,y))`? `f` is not ready to consume a `Maybe Real`. And, we don't want to change every function we could connect with `g` to consume a `Maybe Real`.
    
    Solution: let's **have a special function to "connect"/"compose"/"link" functions**. That way, we can, behind the scenes, adapt the output of one function to feed the following one.
    
    In our case: `g >>= f` (connect/compose `g` to `f`). We want `>>=` to get `g`'s output, inspect it and, in case it is `Nothing` just don't call `f` and return `Nothing`; or on the contrary, extract the boxed `Real` and feed `f` with it. (This algorithm is just the implementation of `>>=` for the `Maybe` type). Also note that `>>=` must be written **only once** per "boxing type" (different box, different adapting algorithm).
    
6.  Many other problems arise which can be solved using this same pattern: 1. Use a "box" to codify/store different meanings/values, and have functions like `g` that return those "boxed values". 2. Have a composer/linker `g >>= f` to help connecting `g`'s output to `f`'s input, so we don't have to change any `f` at all.
    
7.  Remarkable problems that can be solved using this technique are:
    
    *   having a global state that every function in the sequence of functions ("the program") can share: solution `StateMonad`.
        
    *   We don't like "impure functions": functions that yield _different_ output for _same_ input. Therefore, let's mark those functions, making them to return a tagged/boxed value: `IO` monad.
        

Total happiness!
