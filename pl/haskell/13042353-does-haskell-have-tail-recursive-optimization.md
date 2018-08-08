
# Does Haskell have tail-recursive optimization?

## Question
      
I discovered the "time" command in unix today and thought I'd use it to check the difference in runtimes between tail-recursive and normal recursive functions in Haskell.

I wrote the following functions:

    --tail recursive
    fac :: (Integral a) => a -> a
    fac x = fac' x 1 where
        fac' 1 y = y
        fac' x y = fac' (x-1) (x*y) 
    
    --normal recursive
    facSlow :: (Integral a) => a -> a
    facSlow 1 = 1
    facSlow x = x * facSlow (x-1)
    

These are valid keeping in mind they were solely for use with this project, so I didn't bother to check for zeroes or negative numbers.

However, upon writing a main method for each, compiling them, and running them with the "time" command, both had similar runtimes with the _normal_ recursive function edging out the tail recursive one. This is contrary to what I'd heard with regards to tail-recursive optimization in lisp. What's the reason for this?
## Answer
      
Haskell uses lazy-evaluation to implement recursion, so treats anything as a promise to provide a value when needed (this is called a thunk). Thunks get reduced only as much as necessary to proceed, no more. This resembles the way you simplify an expression mathematically, so it's helpful to think of it that way. The fact that evaluation order is _not_ specified by your code allows the compiler to do lots of even cleverer optimisations than just the tail-call elimination youre used to. **Compile with `-O2` if you want optimisation!**

Let's see how we evaluate `facSlow 5` as a case study:

    facSlow 5
    5 * facSlow 4            -- Note that the `5-1` only got evaluated to 4
    5 * (4 * facSlow 3)       -- because it has to be checked against 1 to see
    5 * (4 * (3 * facSlow 2))  -- which definition of `facSlow` to apply.
    5 * (4 * (3 * (2 * facSlow 1)))
    5 * (4 * (3 * (2 * 1)))
    5 * (4 * (3 * 2))
    5 * (4 * 6)
    5 * 24
    120
    

So just as you worried, we have a build-up of numbers before any calculations happen, but _unlike_ you worried, there's no stack of `facSlow` function calls hanging around waiting to terminate - each reduction is applied and goes away, leaving a [stack frame](http://en.wikipedia.org/wiki/Call_stack) in its wake (that is because `(*)` is strict and so triggers the evaluation of its second argument).

Haskell's recursive functions aren't evaluated in a very recursive way! The only stack of calls hanging around are the multiplications themselves. If `(*)` is viewed as a strict data constructor, this is what's known as _guarded_ recursion (although it is usually referred to as such with _non_-strict data constructors, where what's left in its wake are the data constructors - when forced by further access).

Now let's look at the tail-recursive `fac 5`:

    fac 5
    fac' 5 1
    fac' 4 {5*1}       -- Note that the `5-1` only got evaluated to 4
    fac' 3 {4*{5*1}}    -- because it has to be checked against 1 to see
    fac' 2 {3*{4*{5*1}}} -- which definition of `fac'` to apply.
    fac' 1 {2*{3*{4*{5*1}}}}
    {2*{3*{4*{5*1}}}}        -- the thunk "{...}" 
    (2*{3*{4*{5*1}}})        -- is retraced 
    (2*(3*{4*{5*1}}))        -- to create
    (2*(3*(4*{5*1})))        -- the computation
    (2*(3*(4*(5*1))))        -- on the stack
    (2*(3*(4*5)))
    (2*(3*20))
    (2*60)
    120
    

So you can see how the tail recursion by itself hasn't saved you any time or space. Not only does it take more steps overall than `facSlow 5`, it also builds a nested thunk (shown here as `{...}`) \- needing an _extra space_ for it - which describes the future computation, the nested multiplications to be performed.

This thunk is then unraveled by traversing _it_ to the bottom, recreating the computation on the stack. There is also a danger here of causing stack overflow with very long computations, for both versions.

If we want to hand-optimise this, all we need to do is make it strict. You could use the strict application operator `$!` to define

    facSlim :: (Integral a) => a -> a
    facSlim x = facS' x 1 where
        facS' 1 y = y
        facS' x y = facS' (x-1) $! (x*y) 
    

This forces `facS'` to be strict in its second argument. (It's already strict in its first argument because that has to be evaluated to decide which definition of `facS'` to apply.)

Sometimes strictness can help enormously, sometimes it's a big mistake because laziness is more efficient. Here it's a good idea:

    facSlim 5
    facS' 5 1
    facS' 4 5 
    facS' 3 20
    facS' 2 60
    facS' 1 120
    120
    

Which is what you wanted to achieve I think.

Summary
-------

*   If you want to optimise your code, step one is to compile with `-O2`
*   Tail recursion is only good when there's no thunk build-up, and adding strictness usually helps to prevent it, if and where appropriate. This happens when you're building a result that is needed later on all at once.
*   Sometimes tail recursion is a bad plan and guarded recursion is a better fit, i.e. when the result you're building will be needed bit by bit, in portions. See [this question](https://stackoverflow.com/questions/3429634/foldl-is-tail-recursive-so-how-come-foldr-runs-faster-than-foldl) about `foldr` and `foldl` for example, and test them against each other.

Try these two:

    length $ foldl1 (++) $ replicate 1000 
        "The size of intermediate expressions is more important than tail recursion."
    length $ foldr1 (++) $ replicate 1000 
        "The number of reductions performed is more important than tail recursion!!!"
    

`foldl1` is tail recursive, whereas `foldr1` performs guarded recursion so that the first item is immediately presented for further processing/access. (The first "parenthesizes" to the left at once, `(...((s+s)+s)+...)+s`, forcing its input list fully to its end and building a big thunk of future computation much sooner than its full results are needed; the second parenthesizes to the right gradually, `s+(s+(...+(s+s)...))`, consuming the input list bit by bit, so the whole thing is able to operate in constant space, with optimizations).

You might need to adjust the number of zeros depending on what hardware you're using.
    