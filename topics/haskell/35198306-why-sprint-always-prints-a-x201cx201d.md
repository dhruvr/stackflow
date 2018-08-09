
# Why :sprint always prints a &#x201C;_&#x201D;?

## Question
        
    Prelude> let a = 3
    Prelude> :sprint a
    a = _
    Prelude> let c = "ab"
    Prelude> :sprint c
    c = _
    

Why does it always print a `_`? I don't quite get the semantics of the `:sprint` command.

## Answer
        
Haskell is a lazy language. It doesn't evaluate results until they are "needed".

Now, just _printing_ a value causes all of it to be "needed". In other words, if you type an expression in GHCi, it will try to print out the result, which causes it all to be evaluated. Usually that's what you want.

The `sprint` command (which is a GHCi feature, not part of the Haskell language) allows you to see how much of a value has been evaluated at this point.

For example:

    Prelude> let xs = [1..]
    Prelude> :sprint xs
    xs = _
    

So, we just declared `xs`, and it's currently unevaluated. Now let's print out the first element:

    Prelude> head xs
    1
    Prelude> :sprint xs
    xs = 1 : _
    

Now GHCi has evaluated the head of the list, but nothing more.

    Prelude> take 10 xs
    [1,2,3,4,5,6,7,8,9,10]
    Prelude> :sprint xs
    xs = 1 : 2 : 3 : 4 : 5 : 6 : 7 : 8 : 9 : 10 : _
    

Now the first 10 elements are evaluated, but more remain. (Since `xs` is an _infinite list_, that's not surprising.)

You can construct other expressions and evaluate them a bit at a time to see what's going on. This is really part of the GHCi debugger, which lets you step through your code one bit at a time. Especially if your code is getting caught in an infinite loop, you don't want to `print` anything, because that might lock up GHCi. But you still want to see what's going on... hence `sprint`, which lets you see what's evaluated so far.
