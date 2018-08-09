
# Check whether a type is an instance of Show in Haskell?

## Question
        
Suppose I have a simple data type in Haskell for storing a value:

    data V a = V a
    

I want to make V an instance of Show, regardless of a's type. If a is an instance of Show, then `show (V a)` should return `show a` otherwise an error message should be returned. Or in Pseudo-Haskell:

    instance Show (V a) where
        show (V a) = if a instanceof Show
                       then show a
                       else "Some Error."
    

How could this behaviour be implemented in Haskell?

## Answer
        
As I said in a comment, the runtime objects allocated in memory don't have type tags in a Haskell program. There is therefore no universal `instanceof` operation like in, say, Java.

It's also important to consider the implications of the following. In Haskell, to a first approximation (i.e., ignoring some fancy stuff that beginners shouldn't tackle too soon), all runtime function calls are **monomorphic**. I.e., the compiler knows, directly or indirectly, the monomorphic (non-generic) type of every function call in an executable program. Even though your `V` type's `show` function has a generic type:

    -- Specialized to `V a`
    show :: V a -> String  -- generic; has variable `a`
    

...you can't actually write a program that calls the function at runtime without, directly or indirectly, telling the compiler exactly what type `a` will be in every single call. So for example:

    -- Here you tell it directly that `a := Int`
    example1 = show (V (1 :: Int)) 
    
    -- Here you're not saying which type `a` is, but this just "puts off" 
    -- the decision—for `example2` to be called, *something* in the call
    -- graph will have to pick a monomorphic type for `a`.
    example2 :: a -> String
    example2 x = show (V x) ++ example1
    

Seen in this light, hopefully you can spot the problem with what you're asking:

    instance Show (V a) where
        show (V a) = if a instanceof Show
                       then show a
                       else "Some Error."
    

Basically, since the type for the `a` parameter will be known at compilation time for any actual call to your `show` function, there's no point to testing for this type at runtime—you can test for it at compilation time! Once you grasp this, you're led to Will Sewell's suggestion:

    -- No call to `show (V x)` will compile unless `x` is of a `Show` type.
    instance Show a => Show (V a) where ...
    

* * *

**EDIT:** A more constructive answer perhaps might be this: your `V` type needs to be a tagged union of multiple cases. This does require using the `GADTs` extension:

    {-# LANGUAGE GADTs #-}
    
    -- This definition requires `GADTs`.  It has two constructors:
    data V a where
      -- The `Showable` constructor can only be used with `Show` types.
      Showable   :: Show a => a -> V a
      -- The `Unshowable` constructor can be used with any type.
      Unshowable :: a -> V a
    
    instance Show (V a) where
      show (Showable a) = show a
      show (Unshowable a) = "Some Error."
    

But this isn't a runtime check of whether a type is a `Show` instance—your code is responsible for knowing at compilation time where the `Showable` constructor is to be used.
