
# In Haskell, when do we use in with let?

## Question
        
In the following code, the last phrase I can put an `in` in front. Will it change anything?

Another question: If I decide to put `in` in front of the last phrase, do I need to indent it?

I tried without indenting and hugs complains

> Last generator in do {...} must be an expression

    import Data.Char
    groupsOf _ [] = []
    groupsOf n xs = 
        take n xs : groupsOf n ( tail xs )
    
    problem_8 x = maximum . map product . groupsOf 5 $ x
    main = do t <- readFile "p8.log" 
              let digits = map digitToInt $concat $ lines t
              print $ problem_8 digits
    

* * *

### Edit

Ok, so people don't seem to understand what I'm saying. Let me rephrase: are the following two the same, given the context above?

1.

    let digits = map digitToInt $concat $ lines t
    print $ problem_8 digits
    

2.

    let digits = map digitToInt $concat $ lines t
    in print $ problem_8 digits
    

Another question concerning the scope of bindings declared in `let`: I read [here](http://www.haskell.org/tutorial/patterns.html) that:

> `where` Clauses.

Sometimes it is convenient to scope bindings over several guarded equations, which requires a where clause:

    f x y  |  y>z           =  ...
           |  y==z          =  ...
           |  y<z           =  ...
         where z = x*x
    

Note that this cannot be done with a let expression, which only scopes over the expression **which it encloses**.

My question: so, the variable digits shouldn't be visible to the last print phrase. Do I miss something here?

## Answer
        
**Short answer**: Use `let` without `in` in the body of a do-block, and in the part after the `|` in a list comprehension. Anywhere else, use `let ... in ...`.

* * *

The keyword `let` is used in three ways in Haskell.

1.  The first form is a _let-expression_.
    
        let variable = expression in expression
        
    
    This can be used wherever an expression is allowed, e.g.
    
        > (let x = 2 in x*2) + 3
        7
        
    
2.  The second is a _let-statement_. This form is only used inside of do-notation, and does not use `in`.
    
        do statements
           let variable = expression
           statements
        
    
3.  The third is similar to number 2 and is used inside of list comprehensions. Again, no `in`.
    
        > [(x, y) | x <- [1..3], let y = 2*x]
        [(1,2),(2,4),(3,6)]
        
    
    This form binds a variable which is in scope in subsequent generators and in the expression before the `|`.
    

* * *

The reason for your confusion here is that expressions (of the correct type) can be used as statements within a do-block, and `let .. in ..` is just an expression.

Because of the indentation rules of haskell, a line indented further than the previous one means it's a continuation of the previous line, so this

    do let x = 42 in
         foo
    

gets parsed as

    do (let x = 42 in foo)
    

Without indentation, you get a parse error:

    do (let x = 42 in)
       foo
    

In conclusion, never use `in` in a list comprehension or a do-block. It is unneccesary and confusing, as those constructs already have their own form of `let`.
