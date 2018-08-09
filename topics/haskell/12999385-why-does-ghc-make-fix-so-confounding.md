
# Why does GHC make fix so confounding?

## Question
        
Looking at the GHC source code I can see that the definition for **fix** is:

    fix :: (a -> a) -> a
    fix f = let x = f x in x
    

In an example **fix** is used like this:

    fix (\f x -> let x' = x+1 in x:f x')
    

This basically yields a sequence of numbers that increase by one to infinity. For this to happen **fix** must be currying the function that it receives right back to that very function as it's first parameter. It isn't clear to me how the definition of **fix** listed above could be doing that.

This definition is how I came to understand how **fix** works:

    fix :: (a -> a) -> a
    fix f = f (fix f)
    

So now I have two questions:

1.  How does **x** ever come to mean **fix x** in the first definition?
2.  Is there any advantage to using the first definition over the second?

## Answer
        
It's easy to see how this definition works by applying equational reasoning.

    fix :: (a -> a) -> a
    fix f = let x = f x in x
    

What will `x` evaluate to when we try to evaluate `fix f`? It's defined as `f x`, so `fix f = f x`. But what is `x` here? It's `f x`, just as before. So you get `fix f = f x = f (f x)`. Reasoning in this way you get an infinite chain of applications of `f`: `fix f` = `f (f (f (f ...)))`.

Now, substituting `(\f x -> let x' = x+1 in x:f x')` for `f` you get

    fix (\f x -> let x' = x+1 in x:f x')
        = (\f x -> let x' = x+1 in x:f x') (f ...)
        = (\x -> let x' = x+1 in x:((f ...) x'))
        = (\x -> x:((f ...) x + 1))
        = (\x -> x:((\x -> let x' = x+1 in x:(f ...) x') x + 1))
        = (\x -> x:((\x -> x:(f ...) x + 1) x + 1))
        = (\x -> x:(x + 1):((f ...) x + 1))
        = ...
    

**Edit**: Regarding your second question, @is7s pointed out in the comments that the first definition is preferable because it is more efficient.

To find out why, let's look at the Core for `fix1 (:1) !! 10^8`:

    a_r1Ko :: Type.Integer    
    a_r1Ko = __integer 1
    
    main_x :: [Type.Integer]   
    main_x =
      : @ Type.Integer a_r1Ko main_x
    
    main3 :: Type.Integer
    main3 =
      !!_sub @ Type.Integer main_x 100000000
    

As you can see, after the transformations `fix1 (1:)` essentially became `main_x = 1 : main_x`. Note how this definition refers to itself - this is what "tying the knot" means. This self-reference is represented as a simple pointer indirection at runtime:

![fix1](https://i.stack.imgur.com/czWkz.png)

Now let's look at `fix2 (1:) !! 100000000`:

    main6 :: Type.Integer
    main6 = __integer 1
    
    main5
      :: [Type.Integer] -> [Type.Integer]
    main5 = : @ Type.Integer main6
    
    main4 :: [Type.Integer]
    main4 = fix2 @ [Type.Integer] main5
    
    main3 :: Type.Integer
    main3 =
      !!_sub @ Type.Integer main4 100000000
    

Here the `fix2` application is actually preserved:

![fix2](https://i.stack.imgur.com/Qc8H4.png)

The result is that the second program needs to do allocation for each element of the list (but since the list is immediately consumed, the program still effectively runs in constant space):

    $ ./Test2 +RTS -s
       2,400,047,200 bytes allocated in the heap
             133,012 bytes copied during GC
              27,040 bytes maximum residency (1 sample(s))
              17,688 bytes maximum slop
                   1 MB total memory in use (0 MB lost due to fragmentation)
     [...]
    

Compare that to the behaviour of the first program:

    $ ./Test1 +RTS -s          
              47,168 bytes allocated in the heap
               1,756 bytes copied during GC
              42,632 bytes maximum residency (1 sample(s))
              18,808 bytes maximum slop
                   1 MB total memory in use (0 MB lost due to fragmentation)
    [...]
