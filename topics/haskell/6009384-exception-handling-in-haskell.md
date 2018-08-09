
# Exception handling in Haskell

## Question
        
I need help to understand the usage of the three Haskell functions

*   try (`Control.Exception.try :: Exception e => IO a -> IO (Either e a)`)
*   catch (`Control.Exception.catch :: Exception e => IO a -> (e -> IO a) -> IO a`)
*   handle (`Control.Exception.handle :: Exception e => (e -> IO a) -> IO a -> IO a`)

I need to know several things:

1.  When do I use which function?
2.  How do I use this function with some simple example?
3.  Where is the difference between catch and handle? They have nearly the same signature only with a different order.

I will try to write down my trials and hope you can help me:

**try**

I have an example like:

    x = 5 `div` 0
    test = try (print x) :: IO (Either SomeException ())
    

I have two questions:

1.  How can I set a custom error output?
    
2.  What can i do to set all errors to SomeException so I dont must write the `:: IO (Either SomeException())`
    

**catch/try**

Can you show me a short example with a custom error output?

## Answer
        
When do I use which function?
=============================

Here's the recommendation from the Control.Exception documentation:

*   If you want to do some cleanup in the event that an exception is raised, use `finally`, `bracket` or `onException`.
*   To recover after an exception and do something else, the best choice is to use one of the `try` family.
*   ... unless you are recovering from an asynchronous exception, in which case use `catch` or `catchJust`.

try :: Exception e => IO a -> IO (Either e a)
=============================================

`try` takes an `IO` action to run, and returns an `Either`. If the computation succeeded, the result is given wrapped in a `Right` constructor. (Think right as opposed to wrong). If the action threw an exception _of the specified type_, it is returned in a `Left` constructor. If the exception was _not_ of the appropriate type, it continues to propagate up the stack. Specifying `SomeException` as the type will catch all exceptions, which may or may not be a good idea.

Note that if you want to catch an exception from a pure computation, you will have to use `evaluate` to force evaluation within the `try`.

    main = do
        result <- try (evaluate (5 `div` 0)) :: IO (Either SomeException Int)
        case result of
            Left ex  -> putStrLn $ "Caught exception: " ++ show ex
            Right val -> putStrLn $ "The answer was: " ++ show val
    

catch :: Exception e => IO a -> (e -> IO a) -> IO a
===================================================

`catch` is similar to `try`. It first tries to run the specified `IO` action, but if an exception is thrown the handler is given the exception to get an alternative answer.

    main = catch (print $ 5 `div` 0) handler
      where
        handler :: SomeException -> IO ()
        handler ex = putStrLn $ "Caught exception: " ++ show ex
    

**However,** there is one important difference. When using `catch` your handler cannot be interrupted by an asynchroneous exception (i.e. thrown from another thread via `throwTo`). Attempts to raise an asynchroneous exception will block until your handler has finished running.

Note that there is a different `catch` in the Prelude, so you might want to do `import Prelude hiding (catch)`.

handle :: Exception e => (e -> IO a) -> IO a -> IO a
====================================================

`handle` is simply `catch` with the arguments in the reversed order. Which one to use depends on what makes your code more readable, or which one fits better if you want to use partial application. They are otherwise identical.

tryJust, catchJust and handleJust
=================================

Note that `try`, `catch` and `handle` will catch _all_ exceptions of the specified/inferred type. `tryJust` and friends allow you to specify a selector function which filters out which exceptions you specifically want to handle. For example, all arithmetic errors are of type `ArithException`. If you only want to catch `DivideByZero`, you can do:

    main = do
        result <- tryJust selectDivByZero (evaluate $ 5 `div` 0)
        case result of
            Left what -> putStrLn $ "Division by " ++ what
            Right val -> putStrLn $ "The answer was: " ++ show val
      where
        selectDivByZero :: ArithException -> Maybe String
        selectDivByZero DivideByZero = Just "zero"
        selectDivByZero _ = Nothing
    

* * *

A note on purity
================

Note that this type of exception handling can only happen in impure code (i.e. the `IO` monad). If you need to handle errors in pure code, you should look into returning values using `Maybe` or `Either` instead (or some other algebraic datatype). This is often preferable as it's more explicit so you always know what can happen where. Monads like `Control.Monad.Error` makes this type of error handling easier to work with.

* * *

See also:

*   [Control.Exception](http://hackage.haskell.org/packages/archive/base/latest/doc/html/Control-Exception.html)
