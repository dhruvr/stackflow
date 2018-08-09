
# How can a time function exist in functional programming?

## Question
        
I've to admit that I don't know much about functional programming. I read about it from here and there, and so came to know that in functional programming, a function returns the same output, for same input, no matter how many times the function is called. It's exactly like a mathematical function which evaluates to the same output for the same value of the input parameters which involves in the function expression.

For example, consider this:

    f(x,y) = x*x + y; // It is a mathematical function
    

No matter how many times you use `f(10,4)`, its value will always be `104`. As such, wherever you've written `f(10,4)`, you can replace it with `104`, without altering the value of the whole expression. This property is referred to as [referential transparency](http://en.wikipedia.org/wiki/Referential_transparency_(computer_science)) of an expression.

As Wikipedia says ([link](http://en.wikipedia.org/wiki/Functional_programming)),

> Conversely, in functional code, the output value of a function depends only on the arguments that are input to the function, so calling a function f twice with the same value for an argument x will produce the same result f(x) both times.

Can a time function (which returns the _current_ time) exist in functional programming?

*   If yes, then how can it exist? Does it not violate the principle of functional programming? It particularly violates [referential transparency](http://en.wikipedia.org/wiki/Referential_transparency_(computer_science)) which is one of the property of functional programming (if I correctly understand it).
    
*   Or if no, then how can one know the current time in functional programming?

## Answer
        
Another way to explain it is this: no _function_ can get the current time (since it keeps changing), but an _action_ can get the current time. Let's say that `getClockTime` is a constant (or a nullary function, if you like) which represents the _action_ of getting the current time. This _action_ is the same every time no matter when it is used so it is a real constant.

Likewise, let's say `print` is a function which takes some time representation and prints it to the console. Since function calls cannot have side effects in a pure functional language, we instead imagine that it is a function which takes a timestamp and returns the _action_ of printing it to the console. Again, this is a real function, because if you give it the same timestamp, it will return the same _action_ of printing it every time.

Now, how can you print the current time to the console? Well, you have to combine the two actions. So how can we do that? We cannot just pass `getClockTime` to `print`, since print expects a timestamp, not an action. But we can imagine that there is an operator, `>>=`, which _combines_ two actions, one which gets a timestamp, and one which takes one as argument and prints it. Applying this to the actions previously mentioned, the result is... tadaaa... a new action which gets the current time and prints it. And this is incidentally exactly how it is done in Haskell.

    Prelude> System.Time.getClockTime >>= print
    Fri Sep  2 01:13:23 東京 (標準時) 2011
    

So, conceptually, you can view it in this way: A pure functional program does not perform any I/O, it defines an _action_, which the runtime system then executes. The _action_ is the same every time, but the result of executing it depends on the circumstances of when it is executed.

I don't know if this was any clearer than the other explanations, but it sometimes helps me to think of it this way.
