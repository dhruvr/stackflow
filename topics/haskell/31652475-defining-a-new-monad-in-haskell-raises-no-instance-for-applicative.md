
# Defining a new monad in haskell raises no instance for Applicative

## Question
        
I am trying to define a new monad and I am getting a strange error

**newmonad.hs**

newtype Wrapped a = Wrap {unwrap :: a}
instance Monad Wrapped where
  (>>=) (Wrap x) f =  f x
  return x = Wrap x

main = do
  putStrLn "yay"

$ ghc --version
The Glorious Glasgow Haskell Compilation System, version 7.10.1

$ ghc newmonad.hs 
\[1 of 1\] Compiling Main             ( newmonad.hs, newmonad.o )

newmonad.hs:2:10:
    No instance for (Applicative Wrapped)
      arising from the superclasses of an instance declaration
    In the instance declaration for ‚ÄòMonad Wrapped‚Äô

Why do I need to define an instance of `Applicative`?

## Answer
        
This is the Applicative Monad Proposal (AMP). Now whenever you declare something as `Monad`, you also have to declare it as `Applicative` (and therefore `Functor`). Mathematically speaking, every monad _is_ an applicative functor, so this makes sense.

You can do the following to remove the error:

    instance Functor Wrap where
      fmap f (Wrap x) = Wrap (f x)
    
    instance Applicative Wrap where
      pure = Wrap
      Wrap f <*> Wrap x = Wrap (f x)
    

[https://wiki.haskell.org/Functor-Applicative-Monad_Proposal](https://wiki.haskell.org/Functor-Applicative-Monad_Proposal)

**Edit:** Maybe I should point out more clearly that this is a _recent_ thing? The code you posted used to work before, but with _recent_ versions of GHC you'll get an error. It's a breaking change.

**Edit:** The following declarations should work for _any_ monad:

    import Control.Applicative -- Otherwise you can't do the Applicative instance.
    import Control.Monad (liftM, ap)
    
    instance Functor ??? where
      fmap = liftM
    
    instance Applicative ??? where
      pure  = return
      (<*>) = ap
    

Depending on the monad in question, there may be more efficient implementations possible, but this is a simple starting point.
