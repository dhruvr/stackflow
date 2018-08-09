
# Haskell: Equality constraint in instance

## Question
        
I was reading through the announcement of [ClassyPrelude](http://www.yesodweb.com/blog/2012/07/classy-prelude) and got to here:

    instance (b ~ c, CanFilterFunc b a) => CanFilter (b -> c) a where
        filter = filterFunc
    

The writer then mentioned that this would not work:

    instance (CanFilterFunc b a) => CanFilter (c -> c) a where
        filter = filterFunc
    

Which makes sense to me, as `c` is completely unrelated to the constraint on the left.

However, what isn't mentioned in the article and what I don't understand is why this wouldn't work:

    instance (CanFilterFunc b a) => CanFilter (b -> b) a where
        filter = filterFunc
    

Could someone explain why this is different to the first mentioned definition? Perhaps a worked example of GHC type inference would be helpful?

## Answer
        
Michael already gives a good explanation in his blog article, but I'll try to illustrate it with a (contrived, but relatively small) example.

We need the following extensions:

    {-# LANGUAGE FlexibleInstances, TypeFamilies #-}
    

Let's define a class that is simpler than `CanFilter`, with just one parameter. I'm defining two copies of the class, because I want to demonstrate the difference in behaviour between the two instances:

    class Twice1 f where
      twice1 :: f -> f
    
    class Twice2 f where
      twice2 :: f -> f
    

Now, let's define an instance for each class. For `Twice1`, we fix the type variables to be the same directly, and for `Twice2`, we allow them to be different, but add an equality constraint.

    instance Twice1 (a -> a) where
      twice1 f = f . f
    
    instance (a ~ b) => Twice2 (a -> b) where
      twice2 f = f . f
    

In order to show the difference, let us define another overloaded function like this:

    class Example a where
      transform :: Int -> a
    
    instance Example Int where
      transform n = n + 1
    
    instance Example Char where
      transform _ = 'x'
    

Now we are at a point where we can see a difference. Once we define

    apply1 x = twice1 transform x
    apply2 x = twice2 transform x
    

and ask GHC for the inferred types, we get that

    apply1 :: (Example a, Twice1 (Int -> a)) => Int -> a
    apply2 :: Int -> Int
    

Why is that? Well, the instance for `Twice1` only fires when source and target type of the function are the same. For `transform` and the given context, we don't know that. GHC will only apply an instance once the right hand side matches, so we are left with the unresolved context. If we try to say `apply1 0`, there will be a type error saying that there is still not enough information to resolve the overloading. We have to explicitly specify the result type to be `Int` in this case to get through.

However, in `Twice2`, the instance is for any function type. GHC will immediately resolve it (GHC never backtracks, so if an instance clearly matches, it's always chosen), and then try to establish the preconditions: in this case, the equality constraint, which then forces the result type to be `Int` and allows us to resolve the `Example` constraint, too. We can say `apply2 0` without further type annotations.

So this is a rather subtle point about GHC's instance resolution, and the equality constraint here helps GHC's type checker along in a way that requires fewer type annotations by the user.
