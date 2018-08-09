
# Type of fun g x = ys where ys = [x] ++ filter (curry g x) ys?

## Question
        
I'm trying to understand why the type of `fun g x = ys where ys = [x] ++ filter (curry g x) ys` is `((a, a) -> Bool) -> a -> [a]`.

I understand that:

`filter :: (a -> Bool) -> [a] -> [a]` and that `curry :: ((a, b) -> c) -> a -> b -> c`

But I don't understand how to continue.

## Answer
        
The approach below is not necessarily the easiest or fastest, but it's relatively systematic.

* * *

Strictly speaking, you're looking for the type of

    \g -> (\ x -> let ys = (++) [x] (filter (curry g x) ys) in ys)
    

(`let` and `where` are equivalent, but it's sometimes a little easier to reason using `let`), given the types

    filter :: (a -> Bool) -> [a] -> [a]
    curry :: ((a, b) -> c) -> a -> b -> c
    

Don't forget that you're also using

    (++) :: [a] -> [a] -> [a]
    

Let's first look at the 'deepest' part of the syntax tree:

    curry g x
    

We have `g` and `x`, of which we haven't seen before yet, so we'll assume that they have some type:

    g :: t1
    x :: t2
    

We also have `curry`. At every point where these functions occur, the type variables (`a`, `b`, `c`) can be specialized differently, so it's a good idea to replace them with a fresh name every time you use these functions. At this point, `curry` has the following type:

    curry :: ((a1, b1) -> c1) -> a1 -> b1 -> c1
    

We can then only say `curry g x` if the following types can be unified:

    t1  ~  ((a1, b1) -> c1)       -- because we apply curry to g
    t2  ~  a1                     -- because we apply (curry g) to x
    

It's then also safe to assume that

    g :: ((a1, b1) -> c1)
    x :: a1
    ---
    curry g x :: b1 -> c1
    

Let's continue:

    filter (curry g x) ys
    

We see `ys` for the first time, so let's keep it at `ys :: t3` for now. We also have to instantiate `filter`. So at this point, we know

    filter :: (a2 -> Bool) -> [a2] -> [a2]
    ys :: t3
    

Now we must match the types of `filter`'s arguments:

    b1 -> c1  ~  a2 -> Bool
    t3        ~  [a2]
    

The first constraint can be broken down to

    b1  ~  a2
    c1  ~  Bool
    

We now know the following:

    g :: ((a1, a2) -> Bool)
    x :: a1
    ys :: [a2]
    ---
    filter (curry g x) ys :: [a2]
    

Let's continue.

    (++) [x] (filter (curry g x) ys)
    

I won't spend too much time on explaining `[x] :: [a1]`, let's see the type of `(++)`:

    (++) :: [a3] -> [a3] -> [a3]
    

We get the following constraints:

    [a1]  ~  [a3]           -- [x]
    [a2]  ~  [a3]           -- filter (curry g x) ys
    

Since these constraints can be reduced to

    a1  ~  a3
    a2  ~  a2
    

we'll just call all these `a`'s `a1`:

    g :: ((a1, a1) -> Bool)
    x :: a1
    ys :: [a1]
    ---
    (++) [x] (filter (curry g x) ys) :: [a1]
    

For now, I'll ignore that `ys`' type gets generalized, and focus on

    \x -> let { {- ... -} } in ys
    

We know what type we need for `x`, and we know the type of `ys`, so we now know

    g :: ((a1, a1) -> Bool)
    ys :: [a1]
    ---
    (\x -> let { {- ... -} } in ys) :: a1 -> [a1]
    

In a similar fashion, we can conclude that

    (\g -> (\x -> let { {- ... -} } in ys)) :: ((a1, a1) -> Bool) -> a1 -> [a1]
    

At this point, you only have to rename (actually, generalize, because you want to bind it to `fun`) the type variables and you have your answer.
