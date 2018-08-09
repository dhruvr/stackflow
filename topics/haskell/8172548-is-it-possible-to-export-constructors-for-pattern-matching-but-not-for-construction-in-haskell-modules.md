
# Is it possible to export constructors for pattern matching, but not for construction, in Haskell Modules?

## Question
        
A vanilla data type in Haskell has zero or more constructors, each of which plays two roles.

In expressions, it supports introduction, its a function from zero or more arguments to the data type.

In patterns, it supports elimination, its kinda like a function from the data type to Maybe (tuple of argument types).

Is it possible for a module signature to hide the former while exposing the latter?

The use case is this: I have a type, T, whose constructors types alone can sometimes be used to construct nonsense. I have construction functions which can be used to build instances of the type that are guaranteed not to be nonsense. It would make sense to hide the constructors in this case, but it would still be useful for callers to be able to pattern match over the guaranteed-non-nonsense that they build with the construction functions.

I suspect this is impossible, but in case anyone has a way to do it, I though I would ask.

Next best thing is to hide the constructors and create a bunch of functions from T -> Maybe (This, That), T -> Maybe (The, Other, Thing), etc.

## Answer
        
You can use a _view type_ and _view patterns_ to do what you want:

    module ThingModule (Thing, ThingView(..), view) where
    
    data Thing = Foo Thing | Bar Int
    
    data ThingView = FooV Thing | BarV Int
    
    view :: Thing -> ThingView
    view (Foo x) = FooV x
    view (Bar y) = BarV y
    

Note that `ThingView` is not a recursive data type: all the value constructors refer back to `Thing`. So now you can export the value constructors of `ThingView` and keep `Thing` abstract.

Use like this:

    {-# LANGUAGE ViewPatterns #-}
    module Main where
    
    import ThingModule
    
    doSomethingWithThing :: Thing -> Int
    doSomethingWithThing(view -> FooV x) = doSomethingWithThing x
    doSomethingWithThing(view -> BarV y) = y
    

The arrow notation stuff is GHC's [View Patterns](http://hackage.haskell.org/trac/ghc/wiki/ViewPatterns). Note that it requires a language pragma.

Of course you're not required to use view patterns, you can just do all the desugaring by hand:

    doSomethingWithThing :: Thing -> Int
    doSomethingWithThing = doIt . view
      where doIt (FooV x) = doSomethingWithThing x
            doIt (BarV y) = y
    

More
----

Actually we can do a little bit better: There is no reason to duplicate all the value constructors for both `Thing` and `ThingView`

    module ThingModule (ThingView(..), Thing, view) where
    
       newtype Thing = T {view :: ThingView Thing}
       data ThingView a = Foo a | Bar Int
    

Continue useing it the same way as before, but now the pattern matches can use `Foo` and `Bar`.

    {-# LANGUAGE ViewPatterns #-}
    module Main where
    
    import ThingModule
    
    doSomethingWithThing :: Thing -> Int
    doSomethingWithThing(view -> Foo x) = doSomethingWithThing x
    doSomethingWithThing(view -> Bar y) = y
