
# Unsequence Monad function within Haskell

## Question
        
I'm having some real trouble designing the counterfunction of Haskell's `sequence` function, which Hoogle tells me doesn't yet exist. This is how it behaves:

    ghci> sequence [Just 7, Just 8, Just 9]
    Just [7,8,9]
    ghci> sequence [getLine, getLine, getLine]
    hey
    there
    stack exchange
    ["hey","there","stack exchange"] :: IO [String]
    

My problem is making a function like this:

    unsequence :: (Monad m) => m [a] -> [m a]
    

So that it behaves like this:

    ghci> unsequence (Just [7, 8, 9])
    [Just 7, Just 8, Just 9]
    ghci> sequence getLine
    hey
    ['h','e','y'] :: [IO Char] --(This would actually cause an error, but hey-ho.)
    

I don't actually know if that's possible, because I'd be escaping the monad at some point, but I've made a start, though I don't know how to set a breakpoint for this recursive function:

    unsequence m = (m >>= return . head) : unsequence (m >>= return . tail)
    

I realise that I need a breakpoint when the `m` here is equal to `return []`, but not all monads have `Eq` instances, so how can I do this? Is this even possible? If so, why and why not? Please tell me that.

## Answer
        
It is indeed not possible to create an `unsequence` function using monads alone. The reason is:

1.  You can safely and easily create a monadic structure from a value using `return`.
2.  However, it is not safe to remove a value from a monadic structure. For example you can't remove an element from an empty list (i.e. a function of the type `[a] -> a` is not safe).
3.  Hence we have a special function (i.e. `>>=`) which safely removes a value from a monadic structure (if one exists), processes it and returns another safe monadic structure.

Hence it is safe to create a monadic structure from a value. However it is not safe to remove a value from a monadic structure.

Suppose we had a function `extract :: Monad m => m a -> a` which could “safely” remove a value from a monadic structure. We could then implement `unsequence` as follows:

    unsequence :: Monad m => m [a] -> [m a]
    unsequence = map return . extract
    

However, there's no safe way to extract a value from a monadic structure. Hence `unsequence []` and `unsequence Nothing` will return `undefined`.

You can however create an `unsequence` function for structures that are both monadic and comonadic. A `Comonad` is defined as follows:

    class Functor w => Comonad w where
        extract   :: w a -> a
        duplicate :: w a -> w (w a)
        extend    :: (w a -> b) -> w a -> w b
    
        duplicate = extend id
        extend f = fmap f . duplicate
    

A comonadic structure is the opposite of a monadic structure. In particular:

1.  You can safely extract a value from a comonadic structure.
2.  However you can't safely create a new comonadic structure from a value, which is why the `duplicate` function safely creates a new comonadic structure from a value.

Remember that the definition of `unsequence` required both `return` and `extract`? You can't safely create a new comonadic structure from a value (i.e. comonadic structures don't have `return`). Hence the `unsequence` function is defined as follows:

    unsequence :: (Comonad m, Monad m) => m [a] -> [m a]
    unsequence = map return . extract
    

Interestingly `sequence` works on simply monadic structures. So via intuition you might assume that `unsequence` works on simply comonadic structures. However it not so because you need to first extract the list from the comonadic structure and then put each element of the list into a monadic structure.

The general version of the `unsequence` function converts a comonadic list structure to a list of monadic structures:

    unsequence :: (Comonad w, Monad m) => w [a] -> [m a]
    unsequence = map return . extract
    

On the other hand the `sequence` function works on simply monadic structures because you are just folding the list of monadic structures into a monadic list structure by chaining all the monads:

    sequence :: Monad m => [m a] -> m [a]
    sequence = foldr cons (return [])
        where cons mx mxs = do
            x  <- mx
            xs <- mxs
            return $ x:xs
    

Hope that helps.
