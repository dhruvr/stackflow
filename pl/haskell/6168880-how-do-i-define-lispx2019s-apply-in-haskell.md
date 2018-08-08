
# How do I define Lisp&#x2019;s apply in Haskell?

## Question
      
Shouldn’t this definition be allowed in a lazy language like Haskell in which functions are curried?

    apply f [] = f
    apply f (x:xs) = apply (f x) xs
    

It’s basically a function that applies the given function to the given list of arguments and is very easily done in Lisp for example. Are there any workarounds?
## Answer
      
It is hard to give a static type to the `apply` function, since its type depends on the type of the (possibly heterogeneous) list argument. There are at least two one ways to write this function in Haskell that I can think of:

_Using reflection_

We can defer type checking of the application until runtime:

    import Data.Dynamic
    import Data.Typeable
    
    apply :: Dynamic -> [Dynamic] -> Dynamic
    apply f []      = f
    apply f (x:xs)  = apply (f `dynApp` x) xs
    

Note that now the Haskell program may fail with a type error at runtime.

_Via type class recursion_

Using the semi-standard `Text.Printf` trick (invented by augustss, IIRC), a solution can be coded up [in this style](https://stackoverflow.com/questions/5863809/generate-function-of-given-arity-in-haskell-using-type-numbers/5863884#5863884) (exercise). It may not be very useful though, and still requires some trick to hide the types in the list.

_Edit: I couldn't come up with a way to write this, without using dynamic types or hlists/existentials. Would love to see an example_
    