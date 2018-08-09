
# No instance for (Num (Int -&gt; Int)) arising from the literal `5&apos;

## Question
        
I have the following function:

    f :: (Int -> Int) -> Int
    f = undefined
    

Now I want to call `f` with `5` (which is incorrect):

    f 5
    

Obviously, this should not compile, because `5` is not a function from `Int` to `Int`. So I would expect an error message like `Couldn't match expected type Int -> Int with Int`.

But instead I get:

    No instance for (Num (Int -> Int)) arising from the literal `5'
    In the first argument of `f', namely `5'
    In the expression: f 5
    In an equation for `it': it = f 5
    

Why did `Num` appear here?

## Answer
        
`5` is of any type in type class `Num`. These types include `Int`, `Double`, `Integer`, etc.

Functions are not in type class `Num` _by default_. Yet, a `Num` instance for functions might be added by the user, e.g. defining the sum of two functions in a pointwise fashion. In such case, the literal `5` can stand for the constant-five function.

Techncally, the literal stands for `fromInteger 5`, where the `5` is an `Integer` constant. The call `f 5` is therefore actually `f (fromInteger 5)`, which tries to convert five into `Int -> Int`. This requires an instance of `Num (Int -> Int)`.

Hence, GHC does not state in its error that `5` can not be a function (since it _could_ be, if the user declared it such, providing a suitable `fromInteger`). It just states, correctly, that no `Num` instance can be found for integer functions.
