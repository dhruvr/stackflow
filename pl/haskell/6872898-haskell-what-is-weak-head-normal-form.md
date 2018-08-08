
# Haskell: What is Weak Head Normal Form?

## Question
      
What does **Weak Head Normal Form** (WHNF) mean? What does **Head Normal form** (HNF) and **Normal Form** (NF) mean?

[Real World Haskell](http://book.realworldhaskell.org/read/concurrent-and-multicore-programming.html) states:

> The familiar seq function evaluates an expression to what we call head normal form (abbreviated HNF). It stops once it reaches the outermost constructor (the “head”). This is distinct from normal form (NF), in which an expression is completely evaluated.
> 
> You will also hear Haskell programmers refer to weak head normal form (WHNF). For normal data, weak head normal form is the same as head normal form. The difference only arises for functions, and is too abstruse to concern us here.

I have read a few resources and definitions ([Haskell Wiki](http://en.wikibooks.org/wiki/Haskell/Graph_reduction#Weak_Head_Normal_Form) and [Haskell Mail List](http://www.haskell.org/pipermail/beginners/2010-February/003396.html) and [Free Dictionary](http://encyclopedia2.thefreedictionary.com/Weak+Head+Normal+Form)) but I don't get it. Can someone perhaps give an example or provide a layman definition?

I am guessing it would be similar to:

    WHNF = thunk : thunk
    
    HNF = 0 : thunk 
    
    NF = 0 : 1 : 2 : 3 : []
    

How do `seq` and `($!)` relate to WHNF and HNF?

Update
------

I am still confused. I know some of the answers say to ignore HNF. From reading the various definitions it seems that there is no difference between regular data in WHNF and HNF. However, it does seem like there is a difference when it comes to a function. If there was no difference, why is `seq` necessary for `foldl'`?

Another point of confusion is from the Haskell Wiki, which states that `seq` reduces to WHNF, and will do nothing to the following example. Then they say that they have to use `seq` to force the evaluation. Is that not forcing it to HNF?

> Common newbie stack overflowing code:
> 
>     myAverage = uncurry (/) . foldl' (\(acc, len) x -> (acc+x, len+1)) (0,0)
>     
> 
> People who understand seq and weak head normal form (whnf) can immediately understand what goes wrong here. (acc+x, len+1) is already in whnf, so seq, which reduces a value to whnf, does nothing to this. This code will build up thunks just like the original foldl example, they'll just be inside a tuple. The solution is just to force the components of the tuple, e.g.
> 
>     myAverage = uncurry (/) . foldl' 
>               (\(acc, len) x -> acc `seq` len `seq` (acc+x, len+1)) (0,0)
>     

-[Haskell Wiki on Stackoverflow](http://www.haskell.org/haskellwiki/Stack_overflow)
## Answer
      
I'll try to give an explanation in simple terms. As others have pointed out, head normal form does not apply to Haskell, so I will not consider it here.

Normal form
-----------

An expression in normal form is fully evaluated, and no sub-expression could be evaluated any further (i.e. it contains no un-evaluated thunks).

These expressions are all in normal form:

    42
    (2, "hello")
    \x -> (x + 1)
    

These expressions are not in normal form:

    1 + 2                 -- we could evaluate this to 3
    (\x -> x + 1) 2       -- we could apply the function
    "he" ++ "llo"         -- we could apply the (++)
    (1 + 1, 2 + 2)        -- we could evaluate 1 + 1 and 2 + 2
    

Weak head normal form
---------------------

An expression in weak head normal form has been evaluated to the outermost data constructor or lambda abstraction (the _head_). Sub-expressions _may or may not have been evaluated_. Therefore, every normal form expression is also in weak head normal form, though the opposite does not hold in general.

To determine whether an expression is in weak head normal form, we only have to look at the outermost part of the expression. If it's a data constructor or a lambda, it's in weak head normal form. If it's a function application, it's not.

These expressions are in weak head normal form:

    (1 + 1, 2 + 2)       -- the outermost part is the data constructor (,)
    \x -> 2 + 2          -- the outermost part is a lambda abstraction
    'h' : ("e" ++ "llo") -- the outermost part is the data constructor (:)
    

As mentioned, all the normal form expressions listed above are also in weak head normal form.

These expressions are not in weak head normal form:

    1 + 2                -- the outermost part here is an application of (+)
    (\x -> x + 1) 2      -- the outermost part is an application of (\x -> x + 1)
    "he" ++ "llo"        -- the outermost part is an application of (++)
    

Stack overflows
===============

Evaluating an expression to weak head normal form may require that other expressions be evaluated to WHNF first. For example, to evaluate `1 + (2 + 3)` to WHNF, we first have to evaluate `2 + 3`. If evaluating a single expression leads to too many of these nested evaluations, the result is a stack overflow.

This happens when you build up a large expression that does not produce any data constructors or lambdas until a large part of it has been evaluated. These are often caused by this kind of usage of `foldl`:

    foldl (+) 0 [1, 2, 3, 4, 5, 6]
     = foldl (+) (0 + 1) [2, 3, 4, 5, 6]
     = foldl (+) ((0 + 1) + 2) [3, 4, 5, 6]
     = foldl (+) (((0 + 1) + 2) + 3) [4, 5, 6]
     = foldl (+) ((((0 + 1) + 2) + 3) + 4) [5, 6]
     = foldl (+) (((((0 + 1) + 2) + 3) + 4) + 5) [6]
     = foldl (+) ((((((0 + 1) + 2) + 3) + 4) + 5) + 6) []
     = (((((0 + 1) + 2) + 3) + 4) + 5) + 6
     = ((((1 + 2) + 3) + 4) + 5) + 6
     = (((3 + 3) + 4) + 5) + 6
     = ((6 + 4) + 5) + 6
     = (10 + 5) + 6
     = 15 + 6
     = 21
    

Notice how it has to go quite deep before it can get the expression into weak head normal form.

You may wonder, why does not Haskell reduce the inner expressions ahead of time? That is because of Haskell's laziness. Since it cannot be assumed in general that every subexpression will be needed, expressions are evaluated from the outside in.

(GHC has a strictness analyzer that will detect some situations where a subexpression is always needed and it can then evaluate it ahead of time. This is only an optimization, however, and you should not rely on it to save you from overflows).

This kind of expression, on the other hand, is completely safe:

    data List a = Cons a (List a) | Nil
    foldr Cons Nil [1, 2, 3, 4, 5, 6]
     = Cons 1 (foldr Cons Nil [2, 3, 4, 5, 6])  -- Cons is a constructor, stop. 
    

To avoid building these large expressions when we know all the subexpressions will have to be evaluated, we want to force the inner parts to be evaluated ahead of time.

`seq`
=====

`seq` is a special function that is used to force expressions to be evaluated. Its semantics are that `seq x y` means that whenever `y` is evaluated to weak head normal form, `x` is also evaluated to weak head normal form.

It is among other places used in the definition of `foldl'`, the strict variant of `foldl`.

    foldl' f a []     = a
    foldl' f a (x:xs) = let a' = f a x in a' `seq` foldl' f a' xs
    

Each iteration of `foldl'` forces the accumulator to WHNF. It therefore avoids building up a large expression, and it therefore avoids overflowing the stack.

    foldl' (+) 0 [1, 2, 3, 4, 5, 6]
     = foldl' (+) 1 [2, 3, 4, 5, 6]
     = foldl' (+) 3 [3, 4, 5, 6]
     = foldl' (+) 6 [4, 5, 6]
     = foldl' (+) 10 [5, 6]
     = foldl' (+) 15 [6]
     = foldl' (+) 21 []
     = 21                           -- 21 is a data constructor, stop.
    

But as the example on HaskellWiki mentions, this does not save you in all cases, as the accumulator is only evaluated to WHNF. In the example, the accumulator is a tuple, so it will only force evaluation of the tuple constructor, and not `acc` or `len`.

    f (acc, len) x = (acc + x, len + 1)
    
    foldl' f (0, 0) [1, 2, 3]
     = foldl' f (0 + 1, 0 + 1) [2, 3]
     = foldl' f ((0 + 1) + 2, (0 + 1) + 1) [3]
     = foldl' f (((0 + 1) + 2) + 3, ((0 + 1) + 1) + 1) []
     = (((0 + 1) + 2) + 3, ((0 + 1) + 1) + 1)  -- tuple constructor, stop.
    

To avoid this, we must make it so that evaluating the tuple constructor forces evaluation of `acc` and `len`. We do this by using `seq`.

    f' (acc, len) x = let acc' = acc + x
                          len' = len + 1
                      in  acc' `seq` len' `seq` (acc', len')
    
    foldl' f' (0, 0) [1, 2, 3]
     = foldl' f' (1, 1) [2, 3]
     = foldl' f' (3, 2) [3]
     = foldl' f' (6, 3) []
     = (6, 3)                    -- tuple constructor, stop.
    