
# Lifting a higher order function in Haskell

## Question
        
I'm trying to construct a function of type:

    liftSumthing :: ((a -> m b) -> m b) -> (a -> t m b) -> t m b
    

where `t` is a monad transformer. Specifically, I'm interested in doing this:

    liftSumthingIO :: MonadIO m => ((a -> IO b) -> IO b) -> (a -> m b) -> m b
    

I fiddled with some Haskell wizardry libs and but to no avail. How do I get it right, or maybe there is a ready solution somewhere which I did not find?

## Answer
        
This can't be done generically over all `MonadIO` instances because of the `IO` type in a negative position. There are some libraries on hackage that do this for specific instances ([monad-control](http://hackage.haskell.org/package/monad-control), [monad-peel](http://hackage.haskell.org/package/monad-peel)), but there's been some debate over whether they are semantically sound, especially with regards to how they handle exceptions and similar weird `IO`y things.

Edit: Some people seem interested in the positive/negative position distinction. Actually, there's not much to say (and you've probably already heard it, but by a different name). The terminology comes from the world of subtyping.

The intuition behind subtyping is that "`a` is a subtype of `b` (which I'll write `a <= b`) when an `a` can be used anywhere a `b` was expected instead". Deciding subtyping is straightforward in a lot of cases; for products, `(a1, a2) <= (b1, b2)` whenever `a1 <= b1` and `a2 <= b2`, for example, which is a very straightforward rule. But there are a few tricky cases; for example, when should we decide that `a1 -> a2 <= b1 -> b2`?

Well, we have a function `f :: a1 -> a2` and a context expecting a function of type `b1 -> b2`. So the context is going to use `f`'s return value as if it were a `b2`, hence we must require that `a2 <= b2`. The tricky thing is that the context is going to be supplying `f` with a `b1`, even though `f` is going to use it as if it were an `a1`. Hence, we must require that `b1 <= a1` \-\- which looks backwards from what you might guess! We say that `a2` and `b2` are "covariant", or occur in a "positive position", and `a1` and `b1` are "contravariant", or occur in a "negative position".

(Quick aside: why "positive" and "negative"? It's motivated by multiplication. Consider these two types:

    f1 :: ((a1 -> b1) -> c1) -> (d1 -> e1)
    f2 :: ((a2 -> b2) -> c2) -> (d2 -> e2)
    

When should `f1`'s type be a subtype of `f2`'s type? I state these facts (exercise: check this using the rule above):

*   We should have `e1 <= e2`.
*   We should have `d2 <= d1`.
*   We should have `c2 <= c1`.
*   We should have `b1 <= b2`.
*   We should have `a2 <= a1`.

`e1` is in a positive position in `d1 -> e1`, which is in turn in a positive position in the type of `f1`; moreover, `e1` is in a positive position in the type of `f1` overall (since it is covariant, per the fact above). Its position in the whole term is the product of its position in each subterm: positive * positive = positive. Similarly, `d1` is in a negative position in `d1 -> e1`, which is in a positive position in the whole type. negative * positive = negative, and the `d` variables are indeed contravariant. `b1` is in a positive position in the type `a1 -> b1`, which is in a negative position in `(a1 -> b1) -> c1`, which is in a negative position in the whole type. positive * negative * negative = positive, and it's covariant. You get the idea.)

Now, let's take a look at the `MonadIO` class:

    class Monad m => MonadIO m where
        liftIO :: IO a -> m a
    

We can view this as an explicit declaration of subtyping: we are giving a way to make `IO a` be a subtype of `m a` for some concrete `m`. Right away we know we can take any value with `IO` constructors in positive positions and turn them into `m`s. But that's all: we have no way to turn negative `IO` constructors into `m`s -- we need a more interesting class for that.
