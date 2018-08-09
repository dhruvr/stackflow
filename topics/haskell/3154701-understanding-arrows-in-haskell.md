
# Understanding Arrows in Haskell

## Question
        
I've been trying to get a grip on arrows since they're the basis of most [FRP](https://en.wikipedia.org/wiki/Functional_reactive_programming) implementations. I think I understand the basic idea - they're related to monads but store static information at each bind operator so you can walk through a chain of arrows and look at the static information without having to evaluate the whole arrow.

But I get lost at the point where we start discussing first, second, and swap. What do 2-tuples have to do with arrows? Tutorials present the tuple stuff as if it were an obvious next step, but I'm not really seeing the connection.

For that matter, what does the arrow syntax mean intuitively?

## Answer
        
Please take a look in [http://www.cs.yale.edu/homes/hudak/CS429F04/AFPLectureNotes.pdf](http://www.cs.yale.edu/homes/hudak/CS429F04/AFPLectureNotes.pdf), which explains how Arrows work in FRP.

2-tuples are used in defining Arrows because it's needed to represent an arrowized function taking 2 arguments.

In FRP, constants and variables are often represented as arrows which ignores its "input", e.g.

    twelve, eleven :: Arrow f => f p Int
    twelve = arr (const 12)
    eleven = arr (const 11)
    

Function applications are then turned into compositions (`>>>`):

    # (6-) 12
    
    arr (6-) <<< twelve
    

Now how do we turn a 2-argument function into an arrow? For instance

    (+) :: Num a => a -> a -> a
    

due to currying we may treat this as a function returning a function. So

    arr (+) :: (Arrow f, Num a) => f a (a -> a)
    

now let's apply it to a constant

    arr (+)             -- # f     a (a -> a)
      <<< twelve        -- # f b Int
                          :: f b     (Int -> Int)
    
    +----------+      +-----+      +--------------+
    | const 12 |----> | (+) |  ==  | const (+ 12) |
    +----------+      +-----+      +--------------+
    

hey wait, it doesn't work. The result is still an arrow that returns a function, but we expect something akin to `f Int Int`. **We notice that currying fails in Arrow because only composition is allowed.** Therefore we must _uncurry_ the function first

    uncurry :: (a -> b -> c) -> ((a, b) -> c)
    
    uncurry (+) :: Num a => (a, a) -> a
    

Then we have the arrow

    (arr.uncurry) (+) :: (Num a, Arrow f) => f (a, a) a
    

The 2-tuple arises because of this. Then the bunch functions like `&&&` are needed to deal with these 2-tuples.

    (&&&) :: f a b -> f a d -> f a (b, d)
    

then the addition can be correctly performed.

    (arr.uncurry) (+)        -- # f   (a,    a) a
      <<<     twelve         -- # f b  Int
          &&& eleven         -- # f b      Int
                               :: f b           a
    
    +--------+
    |const 12|-----.
    +--------+     |       +-----+      +----------+
                  &&&====> | (+) |  ==  | const 23 |
    +--------+     |       +-----+      +----------+
    |const 11|-----'
    +--------+
    

(Now, why don't we need things like `&&&&` for 3-tuples for functions having 3 arguments? Because a `((a,b),c)` can be used instead.)

* * *

Edit: From John Hughes's original paper _Generalising Monads to Arrows_, it states the reason as

> 4.1 Arrows and Pairs
> --------------------
> 
> However, even though in case of monads the operators `return` and `>>=` are all we need to begin writing useful code, for arrows the analogous operators `arr` and `>>>` are not sufficient. Even the simple monadic addition function that we saw earlier
> 
>        add :: Monad m => m Int -> m Int -> m Int
>        add x y = x >>= \u -> (y >>= \v -> return (u + v))
>     
> 
> cannot yet be expressed in an arrow form. Making dependence on an input explicit, we see that an analogous definition should take the form
> 
>        add :: Arrow a => a b Int -> a b Int -> a b Int
>        add f g = ...
>     
> 
> where we must combine `f` and `g` in sequence. The only sequencing operator available is `>>>`, but `f` and `g` do not have the right types to be composed. Indeed, the `add` function needs to _save the input_ of type `b` across the computation of `f`, so as to be able to supply the same input to `g`. Likewise the result of `f` must be saved across the computation of `g`, so that the two results can eventually be added together and returned. The arrow combinators so far introduced give us no way to save a value across another computation, and so we have no alternative but to introduce another combinator.
