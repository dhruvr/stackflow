
# Composing Monads v. Applicative Functors

## Question
        
The [Typeclassopedia](https://wiki.haskell.org/Typeclassopedia)'s _Monad Transformers_ section explains:

> Unfortunately, monads do not compose as nicely as applicative functors (yet another reason to use Applicative if you don’t need the full power that Monad provides)

Looking at the types of `>>=` and `<*>`, the above statement is not clear to me.

    (<*>) :: Applicative f => f (a -> b) -> f a -> f b
    (>>=) :: Monad m => m a -> (a -> m b) -> m b
    

Please explain the "monads do not compose as nicely as applicative functors."

I read this [answer](https://stackoverflow.com/questions/7040844/applicatives-compose-monads-dont), but could you please give an example to help me understand?

## Answer
        
There are several notions by which types of kind `* -> *` might "compose". The more important one is you can compose them "sequentially".

    newtype Compose f g x = Compose { getCompose :: f (g x) }
    

Here you can see that `Compose` has kind `(* -> *) -> (* -> *) -> (* -> *)` much like any good composition of functors ought to.

So the question is: are there law-abiding instances like the following?

    instance (Applicative f, Applicative g) => Applicative (Compose f g)
    instance (Monad f,       Monad g)       => Monad       (Compose f g)
    

And the short answer as to why monads don't compose as well as applicatives is that while the first instance can be written the second cannot. Let's try!

* * *

We can warm up with `Functor`

    instance (Functor f,     Functor g)     => Functor     (Compose f g) where
      fmap f (Compose fgx) = Compose (fmap (fmap f) fgx)
    

Here we see that because we can `fmap` an `fmap`-ed `f` we can pass it through the layers `f` and `g` like we need to. A similar game is played with `pure`

    instance (Applicative f, Applicative g) => Applicative (Compose f g) where
      pure a = Compose (pure (pure a))
    

and while `(<*>)` appears tricky, if you look carefully it's the exact same trick we used with both `fmap` and `pure`.

      Compose fgf <*> Compose fgx = Compose ((<*>) <$> fgf <*> fgx)
    

In all cases, we can push the operators we need "through" the layers of `f` and `g` exactly as we might hope.

But now let's take a look at `Monad`. Instead of trying to define `Monad` via `(>>=)`, I'm going to instead work via `join`. To implement `Monad` we need to implement

    join :: Compose f g (Compose f g x) -> Compose f g x
    

using

    join_f :: f (f x) -> f x  -- and
    join_g :: g (g x) -> g x
    

or, if we strip off the `newtype` noise, we need

    join :: f (g (f (g x))) -> f (g x)
    

At this point it might be clear what the problem is---we only know how to join _consecutive_ layers of `f`s or `g`s, but here we see them _interwoven_. What you'll find is that we need a _commutativity_ property

    class Commute f g where
      commute :: g (f x) -> f (g x)
    

and now we can implement

    instance (Monad f, Monad g, Commute f g) => Monad (Compose f g)
    

with (the `newtype` agnostic) `join` defined as

    join :: f (g (f (g x))) -> f (g x)
    join fgfgx = fgx where
      ffggx :: f (f (g (g x)))
      ffggx = fmap commute fgfgx
      fggx :: f (g (g x))
      fggx = join_f ffggx
      fgx :: f (g x)
      fgx = fmap join_g fggx
    

* * *

So what's the upshot of all this? `Applicative`s _always_ `Compose`, but `Monad`s `Compose` only when their layers `Commute`.

When can we `commute` layers? Here are some examples

    instance Commute ((->) x) ((->) y) where
      commute = flip
    
    instance Commute ((,) x) ((,) y) where
      commute (y, (x, a)) = (x, (y, a))
    
    instance Commute ((->) x) ((,) y) where
      commute (y, xa) = \x -> (y, xa x)
    
    -- instance Commute ((,) x) ((->) y) does not exist; try to write yourself!
    --
    -- OR:
    -- It turns out that you need to somehow "travel back in time" to make it
    -- work...
    -- 
    -- instance Commute ((,) x) ((->) y) where
    --   commute yxa = ( ..., \y -> let (x, a) = yxa y in a )
