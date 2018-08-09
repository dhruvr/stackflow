
# Does Haskell have variables?

## Question
        
I've frequently heard claims that Haskell doesn't have variables; in particular, [this answer](https://stackoverflow.com/questions/333185/what-type-of-scope-does-haskell-use/333219#333219) claims that it doesn't, and it was upvoted at least nine times and accepted.

So does it have variables or not, and why?

This question also appears to apply ML, F#, OCaml, Erlang, Oz, Lava, and all [SSA](http://en.wikipedia.org/wiki/Static_single_assignment_form) intermediate languages.

## Answer
        
Haskell has immutable variables (variables in the math sense) by default:

     foo x y = x + y * 2
    

By default variables are not _mutable cells_.

Haskell also has mutable cells though, but you enable them explicitly:

     > v <- newIORef 0
     > readIORef v
     0
    
     > writeIORef v 7
     > readIORef v
     7
    

So, _YES_ Haskell has true variables. But it does not use mutable variables by default.
