
# Incomprehensible error message with type families

## Question
        
I'm trying to understand type families without much success. Here's a minimal example:

    {-# LANGUAGE TypeFamilies #-}
    
    class Object obj where
      type Unit obj :: *
      unit :: Unit obj
    
    instance (Object obj, Object obj') => Object (obj, obj') where
      type Unit (obj, obj') = (Unit obj, Unit obj')
      unit = (unit, unit)
    

The intent is fairly transparent I think (trying to define a product category).

This gives me:

    objs.hs:10:10:
        Could not deduce (Unit obj' ~ Unit obj1)
        from the context (Object obj, Object obj')
          bound by the instance declaration at objs.hs:8:10-56
        NB: `Unit' is a type function, and may not be injective
        The type variable `obj1' is ambiguous
        Possible fix: add a type signature that fixes these type variable(s)
        Expected type: Unit (obj, obj')
          Actual type: (Unit obj0, Unit obj1)
        In the expression: (unit, unit)
        In an equation for `unit': unit = (unit, unit)
        In the instance declaration for `Object (obj, obj')'
    

I have tried to add type signatures:

    unit = (unit :: Unit obj, unit :: Unit obj') 
    

but this only makes things worse.

The following modification compiles:

    {-# LANGUAGE TypeFamilies #-}
    
    class Object obj where
      type Unit obj :: *
      unit :: obj -> Unit obj
    
    instance (Object obj, Object obj') => Object (obj, obj') where
      type Unit (obj, obj') = (Unit obj, Unit obj')
      unit (o, o') = (unit o, unit o')
    

but I don't like the superfluous argument for `unit`.

Is it possible to define the argument-less `unit`?

## Answer
        
What you're trying to do is tricky for GHC, because as GHC says in the error message, type families do indeed not need to be injective.

### What does injectivity mean?

A type function `F` is called injective if `F x ~ F y` implies `x ~ y`. If `F` is a normal type constructor, defined via `data`, then this is always true. For type families, however, it does not hold.

For example, there's no problem in defining the following instances, given your definition of `Object`:

    instance Object Int where
      type Unit Int = Int
      unit = 0
    
    instance Object Char where
      type Unit Char = Int
      unit = 1
    

Now if you write `unit :: Int`, then how could GHC possibly determine if it should evaluate to `0` or `1`? Not even writing `unit :: Unit Int` makes it really more clear, because

    Unit Int ~ Int ~ Unit Char
    

so all three types are supposed to be interchangeable. As `Unit` isn't guaranteed to be injective, there's simply no way to uniquely conclude from the knowledge of `Unit x` the knowledge of `x` ...

The consequence is that `unit` can be defined, but not used.

### Solution 1: Dummy or proxy arguments

You have listed the most common way of fixing this problem already. Add an argument that helps GHC to actually determine the type argument in question, by changing the type signature to

    unit :: obj -> Unit obj
    

or

    unit :: Proxy obj -> Unit obj
    

for a suitable definition of `Proxy`, for example simply

    data Proxy a
    

### Solution 2: Manually proving invertibility

A perhaps lesser known option is that you can actually prove to GHC that your type function is invertible.

The way to do that is to define an inverse type family

    type family UnUnit obj :: *
    

and make the invertibility a superclass constraint of the type class:

    class (UnUnit (Unit obj) ~ obj) => Object obj where
      type Unit obj :: *
      unit :: Unit obj
    

Now you have to do extra work. For every instance of the class, you have to define the actual inverse of `Unit` correctly. For example,

    instance (Object obj, Object obj') => Object (obj, obj') where
      type Unit (obj, obj') = (Unit obj, Unit obj')
      unit = (unit, unit)
    
    type instance UnUnit (obj, obj') = (UnUnit obj, UnUnit obj')
    

But given this modification, the definition typechecks. Now if GHC encounters a call the `unit` at some specific type `T` and wants to determine a type `S` such that `Unit S ~ T`, it can apply the superclass constraint to infer that

    S ~ UnUnit (Unit S) ~ UnUnit T
    

If we'd now try to define bad instances as above for `Object Int` and `Object Char` which both map `Unit Int` and `Unit Char` to both be `Int`, that wouldn't work, because we'd have to decide whether `UnObject Int` should be `Int` or `Char`, but couldn't have both ...
