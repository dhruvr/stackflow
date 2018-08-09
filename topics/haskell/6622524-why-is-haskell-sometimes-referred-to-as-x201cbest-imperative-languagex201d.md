
# Why is Haskell (sometimes) referred to as &#x201C;Best Imperative Language&#x201D;?

## Question
        
_(I hope this question is on-topic -- I tried searching for an answer but didn't find a definitive answer. If this happens to be off-topic or already answered, please moderate/remove it.)_

I remember having heard/read the half-joking comment about Haskell being the _best imperative language_ a few times, which of course sounds weird as Haskell is usually best known for its _functional_ features.

So my question is, what qualities/features (if any) of Haskell give reason to justify Haskell being deemed the _best imperative language_ \-\- or is it actually more of a joke?

## Answer
        
I consider it a half-truth. Haskell has an amazing ability to abstract, and that includes abstraction over imperative ideas. For example, Haskell has no built-in imperative while loop, but we can just write it and now it does:

    while :: (Monad m) => m Bool -> m () -> m ()
    while cond action = do
        c <- cond
        if c 
            then action >> while cond action
            else return ()
    

This level of abstraction is difficult for many imperative languages. This can be done in imperative languages that have closures; eg. Python and C#.

But Haskell also has the (highly unique) ability to _characterize allowed side-effects_, using the Monad classes. For example, if we have a function:

    foo :: (MonadWriter [String] m) => m Int
    

This can be an "imperative" function, but we know that it can only do two things:

*   "Output" a stream of strings
*   return an Int

It can't print to the console or establish network connections, etc. Combined with the abstraction ability, you can write functions which act on "any computation that produces a stream", etc.

It's really all about Haskell's abstraction abilities that makes it a very fine imperative language.

However, the false half is the syntax. I find Haskell pretty verbose and awkward to use in an imperative style. Here is an example imperative-style computation using the above `while` loop, which finds the last element of a linked list:

    lastElt :: [a] -> IO a
    lastElt [] = fail "Empty list!!"
    lastElt xs = do
        lst <- newIORef xs
        ret <- newIORef (head xs)
        while (not . null <$> readIORef lst) $ do
            (x:xs) <- readIORef lst
            writeIORef lst xs
            writeIORef ret x
        readIORef ret
    

All that IORef garbage, the double read, having to bind the result of a read, fmapping (`<$>`) to operate on the result of an inline computation... it's all just very complicated looking. It makes a whole lot of sense from a _functional_ perspective, but imperative languages tend to sweep most of these details under the rug to make them easier to use.

Admittedly, perhaps if we used a different `while`-style combinator it would be cleaner. But if you take that philosophy far enough (using a rich set of combinators to express yourself clearly), then you arrive at functional programming again. Imperative-style Haskell just doesn't "flow" like a well-designed imperative language, e.g. python.

In conclusion, with a syntactic face-lift, Haskell might well be the best imperative language. But, by the nature of face lifts, it would be replacing something internally beautiful and real with something externally beautiful and fake.

**EDIT**: Contrast `lastElt` with this python transliteration:

    def last_elt(xs):
        assert xs, "Empty list!!"
        lst = xs
        ret = xs.head
        while lst:
            ret = lst.head
            lst = lst.tail
        return ret 
    

Same number of lines, but each line has quite a bit less noise.

* * *

**EDIT 2**

For what it's worth, this is how a _pure_ replacement in Haskell looks like:

    lastElt = return . last
    

That's it. Or, if you forbid me from using `Prelude.last`:

    lastElt [] = fail "Unsafe lastElt called on empty list"
    lastElt [x] = return x
    lastElt (_:xs) = lastElt xs
    

Or, if you want it to work on any [`Foldable`](https://hackage.haskell.org/package/base-4.7.0.2/docs/Data-Foldable.html) data structure and recognize that you don't actually _need_ `IO` to handle errors:

    import Data.Foldable (Foldable, foldMap)
    import Data.Monoid (Monoid(..), Last(..))
    
    lastElt :: (Foldable t) => t a -> Maybe a
    lastElt = getLast . foldMap (Last . Just)
    

with `Map`, for example:

    λ➔ let example = fromList [(10, "spam"), (50, "eggs"), (20, "ham")] :: Map Int String
    λ➔ lastElt example
    Just "eggs"
    

The `(.)` operator is [function composition](http://en.wikipedia.org/wiki/Function_composition).
