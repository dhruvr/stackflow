
# Haskell : An example of a Foldable which is not a Functor (or not Traversable)?

## Question
        
A [`Foldable`](http://hackage.haskell.org/packages/archive/base/latest/doc/html/Data-Foldable.html) instance is likely to be some sort of container, and so is likely to be a `Functor` as well. Indeed, [this](http://www.haskell.org/haskellwiki/Foldable_and_Traversable) says

> A `Foldable` type is also a container (although the class does not technically require `Functor`, interesting `Foldable`s are all `Functor`s).

So is there an example of a `Foldable` which is not naturally a `Functor` or a `Traversable`? (which perhaps the Haskell wiki page missed :-) )

## Answer
        
Here's a fully parametric example:

    data Weird a = Weird a (a -> a)
    
    instance Foldable Weird where
      foldMap f (Weird a b) = f $ b a
    

`Weird` is not a `Functor` because `a` occurs in a negative position.
