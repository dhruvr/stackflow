
# Handling incremental Data Modeling Changes in Functional Programming

## Question
        
Most of the problems I have to solve in my job as a developer have to do with data modeling. For example in a OOP Web Application world I often have to change the data properties that are in a object to meet new requirements.

If I'm lucky I don't even need to programmatically add new "behavior" code (functions,methods). Instead I can declarative add validation and even UI options by annotating the property (Java).

In Functional Programming it seems that adding new data properties requires lots of code changes because of pattern matching and data constructors (Haskell, ML).

How do I minimize this problem?

This seems to be a recognized problem as [Xavier Leroy states nicely on page 24 of "Objects and Classes vs. Modules"](http://cristal.inria.fr/~xleroy/talks/icfp99.ps.gz) \- To summarize for those that don't have a PostScript viewer it basically says _FP languages are better than OOP languages for adding new behavior over data objects but OOP languages are better for adding new data objects/properties._

**Are there any design pattern used in FP languages to help mitigate this problem?**

I have read Phillip Wadler's [recommendation of using Monads](http://homepages.inf.ed.ac.uk/wadler/papers/essence/essence.ps.gz) to help this modularity problem but I'm not sure I understand how?

## Answer
        
As **Darius Bacon** noted, this is essentially the expression problem, a long-standing issue with no universally accepted solution. The lack of a best-of-both-worlds approach doesn't stop us from sometimes wanting to go one way or the other, though. Now, you asked for a **"design pattern for functional languages"**, so let's take a shot at it. The example that follows is written in Haskell, but isn't necessarily idiomatic for Haskell (or any other language).

First, a quick review of the "expression problem". Consider the following algebraic data type:

    data Expr a = Lit a | Sum (Expr a) (Expr a)
    
    exprEval (Lit x) = x
    exprEval (Sum x y) = exprEval x + exprEval y
    
    exprShow (Lit x) = show x
    exprShow (Sum x y) = unwords ["(", exprShow x, " + ", exprShow y, ")"]
    

This represents simple mathematical expressions, containing only literal values and addition. With the functions we have here, we can take an expression and evaluate it, or show it as a `String`. Now, say we want to add a new function--say, map a function over all the literal values:

    exprMap f (Lit x) = Lit (f x)
    exprMap f (Sum x y) = Sum (exprMap f x) (exprMap f y)
    

Easy! We can keep writing functions all day without breaking a sweat! Algebraic data types are awesome!

In fact, they're so awesome, we want to make our expression type more, errh, expressive. Let's extend it to support multiplication, we'll just... uhh... oh dear, that's going to be awkward, isn't it? We have to modify every function we just wrote. Despair!

In fact, maybe extending the expressions themselves is more interesting than adding functions that use them. So, let's say we're willing to make the trade-off in the other direction. How might we do that?

Well, no sense doing things halfway. Let's **up-end everything and invert the whole program.** What does that mean? Well, this is functional programming, and what's more functional than higher-order functions? What we'll do is **replace the data type representing expression values with one representing actions on the expression**. Instead of choosing a constructor we'll need a record of all possible actions, something like this:

    data Actions a = Actions {
        actEval :: a,
        actMap  :: (a -> a) -> Actions a }
    

So how do we create an expression without a data type? Well, our functions are data now, so I guess our data needs to be functions. We'll make "constructors" using regular functions, returning a record of actions:

    mkLit x = Actions x (\f -> mkLit (f x))
    
    mkSum x y = Actions 
        (actEval x + actEval y) 
        (\f -> mkSum (actMap x f) (actMap y f))
    

Can we add multiplication more easily now? Sure can!

    mkProd x y = Actions 
        (actEval x * actEval y) 
        (\f -> mkProd (actMap x f) (actMap y f))
    

Oh, but wait--we forgot to add an `actShow` action earlier, let's add that in, we'll just... errh, well.

At any rate, what does it look like to use the two different styles?

    expr1plus1 = Sum (Lit 1) (Lit 1)
    action1plus1 = mkSum (mkLit 1) (mkLit 1)
    action1times1 = mkProd (mkLit 1) (mkLit 1)
    

Pretty much the same, when you're not extending them.

As an interesting side note, consider that in the "actions" style, the actual values in the expression are _completely hidden_--the `actEval` field only promises to give us something of the correct type, how it provides it is its own business. Thanks to lazy evaluation, the contents of the field may even be an elaborate computation, performed only on demand. An `Actions a` value is completely opaque to external inspection, presenting only the defined actions to the outside world.

This programming style--replacing simple data with a bundle of "actions" while hiding the actual implementation details in a black box, using constructor-like functions to build new bits of data, being able to interchange very different "values" with the same set of "actions", and so on--is interesting. There's probably a name for it, but I can't quite seem to recall...
