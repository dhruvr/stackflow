
# How do I write, &#x201C;if typeclass a, then a is also an instance of b by this definition.&#x201D;

## Question
        
I have a typeclass `MyClass`, and there is a function in it which produces a `String`. I want to use this to imply an instance of `Show`, so that I can pass types implementing `MyClass` to `show`. So far I have,

    class MyClass a where
        someFunc :: a -> a
        myShow :: a -> String 
    
    instance MyClass a => Show a where
        show a = myShow a
    

which gives the error `Constraint is no smaller than the instance head.` I also tried,

    class MyClass a where
        someFunc :: a -> a
        myShow :: a -> String
    
    instance Show (MyClass a) where
        show a = myShow a
    

which gives the error, `Class`MyClass' used as a type`.

How can I correctly express this relationship in Haskell? Thanks.

I should add that I wish to follow this up with specific instances of `MyClass` that emit specific strings based on their type. For example,

    data Foo = Foo
    data Bar = Bar
    
    instance MyClass Foo where
        myShow a = "foo"
    
    instance MyClass Bar where
        myShow a = "bar"
    
    main = do
        print Foo
        print Bar

## Answer
        
(Edit: leaving the body for posterity, but jump to the end for the real solution)

In the declaration `instance MyClass a => Show a`, let's examine the error "Constraint is no smaller than the instance head." The constraint is the type class constraint to the left of '=>', in this case `MyClass a`. The "instance head" is everything after the class you're writing an instance for, in this case `a` (to the right of `Show`). One of the type inference rules in GHC requires that the constraint have fewer constructors and variables than the head. This is part of what are called the '[Paterson Conditions](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#instance-termination-rules)'. These exist as a guarantee that type checking terminates.

In this case, the constraint is exactly the same as the head, i.e. `a`, so it fails this test. You can remove the Paterson condition checks by enabling [UndecidableInstances](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#undecidable-instances), most likely with the `{-# LANGUAGE UndecidableInstances #-}` pragma.

In this case, you're essentially using your class `MyClass` as a typeclass synonym for the `Show` class. Creating class synonyms like this is one of the canonical uses for the UndecidableInstances extension, so you can safely use it here.

'Undecidable' means that GHC can't prove typechecking will terminate. Although it sounds dangerous, the worst that can happen from enabling UndecidableInstances is that the compiler will loop, eventually terminating after exhausting the stack. If it compiles, then obviously typechecking terminated, so there are no problems. The dangerous extension is IncoherentInstances, which is as bad as it sounds.

Edit: another problem made possible by this approach arises from this situation:

    instance MyClass a => Show a where
    
    data MyFoo = MyFoo ... deriving (Show)
    
    instance MyClass MyFoo where
    

Now there are two instances of Show for `MyFoo`, the one from the deriving clause and the one for MyClass instances. The compiler can't decide which to use, so it will bail out with an error message. If you're trying to make `MyClass` instances of types you don't control that already have `Show` instances, you'll have to use newtypes to hide the already-existing Show instances. Even types without `MyClass` instances will still conflict because the definition `instance MyClass => Show a` because the definition actually provides an implementation for all possible `a` (the context check comes in later; its not involved with instance selection)

So that's the error message and how UndecidableInstances makes it go away. Unfortunately it's a lot of trouble to use in actual code, for reasons Edward Kmett explains. The original impetus was to avoid specifying a `Show` constraint when there's already a `MyClass` constraint. Given that, what I would do is just use `myShow` from `MyClass` instead of `show`. You won't need the `Show` constraint at all.
