
# When are type signatures necessary in Haskell?

## Question
        
Many introductory texts will tell you that in Haskell type signatures are "almost always" optional. Can anybody quantify the "almost" part?

As far as I can tell, the _only_ time you need an explicit signature is to disambiguate type classes. (The canonical example being `read . show`.) Are there other cases I haven't thought of, or is this it?

(I'm aware that if you go beyond Haskell 2010 there are plenty for exceptions. For example, GHC will never infer rank-N types. But rank-N types are a language extension, not part of the official standard \[yet\].)

## Answer
        
### Monomorphism restriction

If you have `MonomorphismRestriction` enabled, then sometimes you will need to add a type signature to get the most general type:

    {-# LANGUAGE MonomorphismRestriction #-}
    -- myPrint :: Show a => a -> IO ()
    myPrint = print
    main = do
      myPrint ()
      myPrint "hello"
    

This will fail because `myPrint` is monomorphic. You would need to uncomment the type signature to make it work, or disable `MonomorphismRestriction`.

### Phantom constraints

When you put a polymorphic value with a constraint into a tuple, the tuple itself becomes polymorphic and has the same constraint:

    myValue :: Read a => a
    myValue = read "0"
    
    myTuple :: Read a => (a, String)
    myTuple = (myValue, "hello")
    

We know that the constraint affects the first part of the tuple but does _not_ affect the second part. The type system doesn't know that, unfortunately, and will complain if you try to do this:

    myString = snd myTuple
    

Even though intuitively one would expect `myString` to be just a `String`, the type checker _needs_ to specialize the type variable `a` and figure out whether the constraint is actually satisfied. In order to make this expression work, one would need to annotate the type of either `snd` or `myTuple`:

    myString = snd (myTuple :: ((), String))
