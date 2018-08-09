
# Pointer equality in Haskell?

## Question
        
Is there any notion of pointer quality in Haskell? == requires things to be deriving Eq, and I have something which contains a (Value -> IO Value), and neither -> nor IO derive Eq.

EDIT: I'm creating an interpreter for another language which _does_ have pointer equality, so I'm trying to model this behavior while still being able to use Haskell functions to model closures.

EDIT: Example: I want a function `special` that would do this:

    > let x a = a * 2
    > let y = x
    > special x y
    True
    > let z a = a * 2
    > special x z
    False

## Answer
        
**EDIT**: Given your example, you could model this with the IO monad. Just assign your functions to IORefs and compare them.

    Prelude Data.IORef> z <- newIORef (\x -> x)
    Prelude Data.IORef> y <- newIORef (\x -> x)
    Prelude Data.IORef> z == z
    True
    Prelude Data.IORef> z == y
    False
