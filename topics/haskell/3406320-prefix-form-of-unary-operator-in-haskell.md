
# Prefix form of unary operator in Haskell

## Question
        
In GHCi:

> 1.  Prelude> (+3) 2  
>     5
> 2.  Prelude> (*3) 2  
>     6
> 3.  Prelude> (/3) 2  
>     0.6666666666666666
> 4.  Prelude> (-3) 2  
>     No instance for (Num (t -> t1))  
>     arising from the literal `3' at <interactive>:1:2  
>     Possible fix: add an instance declaration for (Num (t -> t1))  
>     In the expression: 3  
>     In the expression: (- 3) 2  
>     In the definition of`it': it = (- 3) 2

How can I correct the last one to make it return -1?

## Answer
        
Haskell's grammar doesn't allow you to use `-` like that. Use the `subtract` function instead:

    (subtract 3) 2
