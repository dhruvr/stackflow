
# How do I use fix, and how does it work?

## Question
        
I was a bit confused by the documentation for `fix` (although I think I understand what it's supposed to do now), so I looked at the source code. That left me more confused:

    fix :: (a -> a) -> a
    fix f = let x = f x in x
    

How exactly does this return a fixed point?

I decided to try it out at the command line:

    Prelude Data.Function> fix id
    ...
    

And it hangs there. Now to be fair, this is on my old macbook which is kind of slow. However, this function can't be _too_ computationally expensive since anything passed in to id gives that same thing back (not to mention that it's eating up no CPU time). What am I doing wrong?

## Answer
        
You are doing nothing wrong. `fix id` is an infinite loop.

When we say that `fix` returns the least fixed point of a function, we mean that in the [domain theory](http://en.wikibooks.org/wiki/Haskell/Denotational_semantics) sense. So `fix (\x -> 2*x-1)` is _not_ going to return `1`, because although `1` is a fixed point of that function, it is not the _least_ one in the domain ordering.

I can't describe the domain ordering in a mere paragraph or two, so I will refer you to the domain theory link above. It is an excellent tutorial, easy to read, and quite enlightening. I highly recommend it.

For the view from 10,000 feet, `fix` is a higher-order function which encodes the idea of _recursion_. If you have the expression:

    let x = 1:x in x
    

Which results in the infinite list `[1,1..]`, you could say the same thing using `fix`:

    fix (\x -> 1:x)
    

(Or simply `fix (1:)`), which says find me a fixed point of the `(1:)` function, IOW a value `x` such that `x = 1:x`... just like we defined above. As you can see from the definition, `fix` is nothing more than this idea -- recursion encapsulated into a function.

It is a truly general concept of recursion as well -- you can write any recursive function this way, [including functions that use polymorphic recursion](http://h2.jaguarpaw.co.uk/posts/polymorphic-recursion-combinator/). So for example the typical fibonacci function:

    fib n = if n < 2 then n else fib (n-1) + fib (n-2)
    

Can be written using `fix` this way:

    fib = fix (\f -> \n -> if n < 2 then n else f (n-1) + f (n-2))
    

Exercise: expand the definition of `fix` to show that these two definitions of `fib` are equivalent.

But for a full understanding, read about domain theory. It's really cool stuff.
