
# What advantage does Monad give us over an Applicative?

## Question
        
I've read [this article](https://www.fpcomplete.com/school/advanced-haskell-1/functors-applicative-functors-and-monads), but didn't understand last section.

The author says that Monad gives us context sensitivity, but it's possible to achieve the same result using only an Applicative instance:

    let maybeAge = (\futureYear birthYear -> if futureYear < birthYear
        then yearDiff birthYear futureYear
        else yearDiff futureYear birthYear) <$> (readMay futureYearString) <*> (readMay birthYearString)
    

It's uglier for sure without do-syntax, but beside that I don't see why we need Monad. Can anyone clear this up for me?

## Answer
        
Here's a couple of functions that use the `Monad` interface.

    ifM :: Monad m => m Bool -> m a -> m a -> m a
    ifM c x y = c >>= \z -> if z then x else y
    
    whileM :: Monad m => (a -> m Bool) -> (a -> m a) -> a -> m a
    whileM p step x = ifM (p x) (step x >>= whileM p step) (return x)
    

You can't implement them with the `Applicative` interface. But for the sake of enlightenment, let's try and see where things go wrong. How about..

    import Control.Applicative
    
    ifA :: Applicative f => f Bool -> f a -> f a -> f a
    ifA c x y = (\c' x' y' -> if c' then x' else y') <$> c <*> x <*> y
    

Looks good! It has the right type, it must be the same thing! Let's just check to make sure..

    *Main> ifM (Just True) (Just 1) (Just 2)
    Just 1
    *Main> ifM (Just True) (Just 1) (Nothing)
    Just 1
    *Main> ifA (Just True) (Just 1) (Just 2)
    Just 1
    *Main> ifA (Just True) (Just 1) (Nothing)
    Nothing
    

And there's your first hint at the difference. You can't write a function using just the `Applicative` interface that replicates `ifM`.

If you divide this up into thinking about values of the form `f a` as being about "effects" and "results" (both of which are very fuzzy approximate terms that are the best terms available, but not very good), you can improve your understanding here. In the case of values of type `Maybe a`, the "effect" is success or failure, as a computation. The "result" is a value of type `a` that might be present when the computation completes. (The meanings of these terms depends heavily on the concrete type, so don't think this is a valid description of anything other than `Maybe` as a type.)

Given that setting, we can look at the difference in a bit more depth. The `Applicative` interface allows the "result" control flow to be dynamic, but it requires the "effect" control flow to be static. If your expression involves 3 computations that can fail, the failure of any one of them causes the failure of the whole computation. The `Monad` interface is more flexible. It allows the "effect" control flow to depend on the "result" values. `ifM` chooses which argument's "effects" to include in its own "effects" based on its first argument. This is the huge fundamental difference between `ifA` and `ifM`.

There's something even more serious going on with `whileM`. Let's try to make `whileA` and see what happens.

    whileA :: Applicative f => (a -> f Bool) -> (a -> f a) -> a -> f a
    whileA p step x = ifA (p x) (whileA p step <*> step x) (pure x)
    

Well.. What happens is a compile error. `(<*>)` doesn't have the right type there. `whileA p step` has the type `a -> f a` and `step x` has the type `f a`. `(<*>)` isn't the right shape to fit them together. For it to work, the function type would need to be `f (a -> a)`.

You can try lots more things - but you'll eventually find that `whileA` has no implementation that works anything even close to the way `whileM` does. I mean, you can implement the type, but there's just no way to make it both loop and terminate.

Making it work _requires_ either `join` or `(>>=)`. (Well, or one of the many equivalents of one of those) And those the extra things you get out of the `Monad` interface.
