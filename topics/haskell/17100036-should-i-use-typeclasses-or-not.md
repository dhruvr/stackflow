
# Should I use typeclasses or not?

## Question
        
I have some difficulties to understand when use and when not use typeclass in my code. I mean _create my own_, and not _use already defined_ typeclasses, of course. By example (very stupid example), should I do:

    data Cars = Brakes | Wheels | Engine
    data Computers = Processor | RAM | HardDrive  
    
    class Repairable a where
        is_reparaible :: a -> Bool
    
    instance Repairable Cars where
        is_repairable (Brakes) = True
        is_repairable (Wheels) = False
        is_repairable (Engine) = False
    
    instance Repairable Computers where
        is_repairable (Processor) = False
        is_repairable (RAM)       = False
        is_repairable (HardDrive) = True
    
    checkState :: (Reparaible a) => a -> ... 
    checkState a = ...
    

(Obviously, this is an stupid, incomplete example).

But this is a lot for a little use, no? Why I shouldn't do something simple and only defining functions without defining new data types and typeclasses (with their instances).

This example is too simple, but in facts I often see somethings like that (new data types+typeclasses+instances) when I browse Haskell code on github instead of only defining functions.

So, when I should create new data types, typeclasses etc and when should I use functions?

Thanks.

## Answer
        
> Why I shouldn't do something simple and only defining functions without defining new data types and typeclasses (with their instances).

Why indeed? You could just define:

    checkState :: (a -> Bool) -> (a -> b) -> (a -> b) -> a -> b
    checkState is_repairable repairs destroy a
        = if (is_repairable a) then repairs a else destroy a
    

People misuse type classes all the time. It doesn't mean that it's idiomatic.

To answer your more general question, here are some rules of thumb for when to use type classes and when not to use them:

Use type classes if:

*   There is only one correct behavior per given type
    
*   The type class has associated equations (i.e. "laws") that all instances must satisfy
    

Don't use type classes if:

*   You are trying to just namespace things. That's what modules and namespaces are for.
    
*   A person using your type class cannot reason about how it will behave without looking at the source code of the instances
    
*   You find that the extensions you have to turn on are getting out of control
