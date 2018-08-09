
# Variadic compose function?

## Question
        
I'm trying to write a variadic function composition function. Which is basically the `(.)` except that the second argument function is variadic. This should allow expressions like:

    map even . zipWith (+)
    

or just

    map even . zipWith
    

Currently what I've reached works if I add `IncoherentInstances` and requires a non-polymorphic instance for the first argument function.

    {-# LANGUAGE FlexibleInstances, OverlappingInstances, MultiParamTypeClasses, 
    FunctionalDependencies, UndecidableInstances, KindSignatures #-}
    
    class Comp a b c d | c -> d where
        comp :: (a -> b) -> c -> d
    
    instance Comp a b (a :: *) (b :: *) where
        comp f g = f g
    
    instance Comp c d b e => Comp c d (a -> b) (a -> e) where
        comp f g = comp f . g
    

Any ideas? Is it even possible?

## Answer
        
It is possible to type-hack it into working with polymorphic functions:

    {-# LANGUAGE FlexibleInstances, MultiParamTypeClasses,
      IncoherentInstances, UndecidableInstances,
      FunctionalDependencies, TypeFamilies,
      NoMonomorphismRestriction #-}
    
    
    class Comp a b c | a b -> c where
        (...) :: a -> b -> c
    
    instance (a ~ c, r ~ b) => Comp (a -> b) c r where
        f ... g = f g
    
    instance (Comp (a -> b) d r1, r ~ (c -> r1)) => Comp (a -> b) (c -> d) r where
        f ... g = \c -> f ... g c
    
    t1 = map even ... zipWith (+)
    t2 = map even ... zipWith
    t3 = (+1) ... foldr
    

But I doubt you can avoid `IncoherentInstances`
