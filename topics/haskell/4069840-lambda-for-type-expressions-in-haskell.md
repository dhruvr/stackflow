
# Lambda for type expressions in Haskell?

## Question
        
Does Haskell, or a specific compiler, have anything like type-level lambdas (if that's even a term)?

To elaborate, say I have a parametrized type `Foo a b` and want `Foo _ b` to be an instance of, say, Functor. Is there any mechanism that would let me do something akin to

    instance Functor (\a -> Foo a b) where
    ...
    

?

## Answer
        
From TypeCompose:

    newtype Flip (~>) b a = Flip { unFlip :: a ~> b }
    

[http://hackage.haskell.org/packages/archive/TypeCompose/0.6.3/doc/html/Control-Compose.html#t:Flip](http://hackage.haskell.org/packages/archive/TypeCompose/0.6.3/doc/html/Control-Compose.html#t:Flip)

Also, if something is a Functor in two arguments, you can make it a bifunctor:

[http://hackage.haskell.org/packages/archive/category-extras/0.44.4/doc/html/Control-Bifunctor.html](http://hackage.haskell.org/packages/archive/category-extras/0.44.4/doc/html/Control-Bifunctor.html)

(or, in a later category-extras, a more general version: [http://hackage.haskell.org/packages/archive/category-extras/0.53.5/doc/html/Control-Functor.html#t:Bifunctor](http://hackage.haskell.org/packages/archive/category-extras/0.53.5/doc/html/Control-Functor.html#t:Bifunctor))
