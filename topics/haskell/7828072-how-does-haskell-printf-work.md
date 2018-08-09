
# How does Haskell printf work?

## Question
        
Haskell's type safety is second to none only to dependently-typed languages. But there is some deep magic going on with [Text.Printf](http://hackage.haskell.org/packages/archive/base/latest/doc/html/Text-Printf.html) that seems rather type-wonky.

    > printf "%d\n" 3
    3
    > printf "%s %f %d" "foo" 3.3 3
    foo 3.3 3
    

What is the deep magic behind this? How can the `Text.Printf.printf` function take variadic arguments like this?

**What is the general technique used to allow for variadic arguments in Haskell, and how does it work?**

(Side note: some type safety is apparently lost when using this technique.)

    > :t printf "%d\n" "foo"
    printf "%d\n" "foo" :: (PrintfType ([Char] -> t)) => t

## Answer
        
The trick is to use type classes. In the case of `printf`, the key is the `PrintfType` type class. It does not expose any methods, but the important part is in the types anyway.

    class PrintfType r
    printf :: PrintfType r => String -> r
    

So `printf` has an overloaded return type. In the trivial case, we have no extra arguments, so we need to be able to instantiate `r` to `IO ()`. For this, we have the instance

    instance PrintfType (IO ())
    

Next, in order to support a variable number of arguments, we need to use recursion at the instance level. In particular we need an instance so that if `r` is a `PrintfType`, a function type `x -> r` is also a `PrintfType`.

    -- instance PrintfType r => PrintfType (x -> r)
    

Of course, we only want to support arguments which can actually be formatted. That's where the second type class `PrintfArg` comes in. So the actual instance is

    instance (PrintfArg x, PrintfType r) => PrintfType (x -> r)
    

Here's a simplified version which takes any number of arguments in the `Show` class and just prints them:

    {-# LANGUAGE FlexibleInstances #-}
    
    foo :: FooType a => a
    foo = bar (return ())
    
    class FooType a where
        bar :: IO () -> a
    
    instance FooType (IO ()) where
        bar = id
    
    instance (Show x, FooType r) => FooType (x -> r) where
        bar s x = bar (s >> print x)
    

Here, `bar` takes an IO action which is built up recursively until there are no more arguments, at which point we simply execute it.

    *Main> foo 3 :: IO ()
    3
    *Main> foo 3 "hello" :: IO ()
    3
    "hello"
    *Main> foo 3 "hello" True :: IO ()
    3
    "hello"
    True
    

QuickCheck also uses the same technique, where the `Testable` class has an instance for the base case `Bool`, and a recursive one for functions which take arguments in the `Arbitrary` class.

    class Testable a
    instance Testable Bool
    instance (Arbitrary x, Testable r) => Testable (x -> r)
