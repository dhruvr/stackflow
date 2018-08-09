
# Haskell: Where vs. Let

## Question
        
I am new to Haskell and I am very confused by **Where** vs. **Let**. They both seem to provide a similar purpose. I have read a few comparisons between **Where** vs. **Let** but I am having trouble discerning when to use each. Could someone please provide some context or perhaps a few examples that demonstrate when to use one over the other?

> Where vs. Let
> 
> A `where` clause can only be defined at the level of a function definition. Usually, that is identical to the scope of `let` definition. _The only difference is when guards are being used_. The scope of the `where` clause extends over all guards. In contrast, the scope of a `let` expression is only the current function clause and guard, if any.

[Haskell Cheat Sheet](http://cheatsheet.codeslower.com/CheatSheet.pdf)

The [Haskell Wiki](http://www.haskell.org/haskellwiki/Let_vs._Where) is very detailed and provides various cases but it uses hypothetical examples. I find its explanations too brief for a beginner.

**Advantages of Let**:

    f :: State s a
    f = State $ \x -> y
       where y = ... x ...
    

[Control.Monad.State](http://www.haskell.org/haskellwiki/State_Monad)

> will not work, because where refers to the pattern matching f =, where no x is in scope. In contrast, if you had started with let, then you wouldn't have trouble.

[Haskell Wiki on Advantages of Let](http://www.haskell.org/haskellwiki/Let_vs._Where#Advantages_of_let)

    f :: State s a
    f = State $ \x ->
       let y = ... x ...
       in  y
    

**Advantages of Where**:

    f x
      | cond1 x   = a
      | cond2 x   = g a
      | otherwise = f (h x a)
      where
        a = w x
    
    f x
      = let a = w x
        in case () of
            _ | cond1 x   = a
              | cond2 x   = g a
              | otherwise = f (h x a)
    

[**Declaration vs. Expression**](http://www.haskell.org/haskellwiki/Declaration_vs._expression_style)

The Haskell wiki mentions that the **Where** clause is declarative while the **Let** expression is expressive. Aside from style how do they perform differently?

    Declaration style                     | Expression-style
    --------------------------------------+---------------------------------------------
    where clause                          | let expression
    arguments LHS:     f x = x*x          | Lambda abstraction: f = \x -> x*x
    Pattern matching:  f [] = 0           | case expression:    f xs = case xs of [] -> 0
    Guards:            f [x] | x>0 = 'a'  | if expression:      f [x] = if x>0 then 'a' else ...
    

1.  In the first example why is the **Let** in scope but **Where** is not?
2.  Is it possible to apply **Where** to the first example?
3.  Can some apply this to real examples where the variables represent actual expressions?
4.  Is there a general rule of thumb to follow when to use each?

* * *

Update
------

For those that come by this thread later on I found the best explanation to be found here: "[A Gentle Introduction to Haskell](http://www.cse.iitb.ac.in/~as/fpcourse/haskell98_tutorial/patterns.html)".

> Let Expressions.
> 
> Haskell's let expressions are useful whenever a nested set of bindings is required. As a simple example, consider:
> 
>     let y   = a*b
>         f x = (x+y)/y
>     in f c + f d
>     
> 
> The set of bindings created by a let expression is mutually recursive, and pattern bindings are treated as lazy patterns (i.e. they carry an implicit ~). The only kind of declarations permitted are type signatures, function bindings, and pattern bindings.
> 
> Where Clauses.
> 
> Sometimes it is convenient to scope bindings over several guarded equations, which requires a where clause:
> 
>     f x y  |  y>z           =  ...
>            |  y==z          =  ...
>            |  y<z           =  ...
>          where z = x*x
>     
> 
> Note that this cannot be done with a let expression, which only scopes over the expression which it encloses. A where clause is only allowed at the top level of a set of equations or case expression. The same properties and constraints on bindings in let expressions apply to those in where clauses. These two forms of nested scope seem very similar, but remember that a let expression is an expression, whereas a where clause is not -- it is part of the syntax of function declarations and case expressions.

## Answer
        
1: The problem in the example

    f :: State s a
    f = State $ \x -> y
        where y = ... x ...
    

is the parameter `x`. Things in the `where` clause can refer only to the parameters of the function `f` (there are none) and things in outer scopes.

2: To use a `where` in the first example, you can introduce a second named function that takes the `x` as a parameter, like this:

    f = State f'
    f' x = y
        where y = ... x ...
    

or like this:

    f = State f'
        where
        f' x = y
            where y = ... x ...
    

3: Here is a complete example without the `...`'s:

    module StateExample where
    
    data State a s = State (s -> (a, s))
    
    f1 :: State Int (Int, Int)
    f1 = State $ \state@(a, b) ->
        let
            hypot = a^2 + b^2
            result = (hypot, state)
        in result
    
    f2 :: State Int (Int, Int)
    f2 = State f
        where
        f state@(a, b) = result
            where
            hypot = a^2 + b^2
            result = (hypot, state)
    

4: When to use `let` or `where` is a matter of taste. I use `let` to emphasize a computation (by moving it to the front) and `where` to emphasize the program flow (by moving the computation to the back).
