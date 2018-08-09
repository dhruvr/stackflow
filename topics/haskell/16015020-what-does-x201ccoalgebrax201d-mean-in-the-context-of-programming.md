
# What does &#x201C;coalgebra&#x201D; mean in the context of programming?

## Question
        
I have heard the term "coalgebras" several times in functional programming and PLT circles, especially when the discussion is about objects, comonads, lenses, and such. Googling this term gives pages that give mathematical description of these structures which is pretty much incomprehensible to me. Can anyone please explain what coalgebras mean in the context of programming, what is their significance, and how they relate to objects and comonads?

## Answer
        
Algebras
========

I think the place to start would be to understand the idea of an **algebra**. This is just a generalization of algebraic structures like groups, rings, monoids and so on. Most of the time, these things are introduced in terms of sets, but since we're among friends, I'll talk about Haskell types instead. (I can't resist using some Greek letters though—they make everything look cooler!)

An algebra, then, is just a type `τ` with some functions and identities. These functions take differing numbers of arguments of type `τ` and produce a `τ`: uncurried, they all look like `(τ, τ,…, τ) → τ`. They can also have "identities"—elements of `τ` that have special behavior with some of the functions.

The simplest example of this is the monoid. A monoid is any type `τ` with a function `mappend ∷ (τ, τ) → τ` and an identity `mzero ∷ τ`. Other examples include things like groups (which are just like monoids except with an extra `invert ∷ τ → τ` function), rings, lattices and so on.

All the functions operate on `τ` but can have different arities. We can write these out as `τⁿ → τ`, where `τⁿ` maps to a tuple of `n` `τ`. This way, it makes sense to think of identities as `τ⁰ → τ` where `τ⁰` is just the empty tuple `()`. So we can actually simplify the idea of an algebra now: it's just some type with some number of functions on it.

An algebra is just a common pattern in mathematics that's been "factored out", just like we do with code. People noticed that a whole bunch of interesting things—the aforementioned monoids, groups, lattices and so on—all follow a similar pattern, so they abstracted it out. The advantage of doing this is the same as in programming: it creates reusable proofs and makes certain kinds of reasoning easier.

F-Algebras
----------

However, we're not quite done with factoring. So far, we have a bunch of functions `τⁿ → τ`. We can actually do a neat trick to combine them all into one function. In particular, let's look at monoids: we have `mappend ∷ (τ, τ) → τ` and `mempty ∷ () → τ`. We can turn these into a single function using a sum type—`Either`. It would look like this:

    op ∷ Monoid τ ⇒ Either (τ, τ) () → τ
    op (Left (a, b)) = mappend (a, b)
    op (Right ())    = mempty
    

We can actually use this transformation repeatedly to combine _all_ the `τⁿ → τ` functions into a single one, for _any_ algebra. (In fact, we can do this for any number of functions `a → τ`, `b → τ` and so on for _any_ `a, b,…`.)

This lets us talk about algebras as a type `τ` with a _single_ function from some mess of `Either`s to a single `τ`. For monoids, this mess is: `Either (τ, τ) ()`; for groups (which have an extra `τ → τ` operation), it's: `Either (Either (τ, τ) τ) ()`. It's a different type for every different structure. So what do all these types have in common? The most obvious thing is that they are all just sums of products—algebraic data types. For example, for monoids, we could create a monoid argument type that works for _any_ monoid τ:

    data MonoidArgument τ = Mappend τ τ -- here τ τ is the same as (τ, τ)
                          | Mempty      -- here we can just leave the () out
    

We can do the same thing for groups and rings and lattices and all the other possible structures.

What else is special about all these types? Well, they're all `Functors`! E.g.:

    instance Functor MonoidArgument where
      fmap f (Mappend τ τ) = Mappend (f τ) (f τ)
      fmap f Mempty        = Mempty
    

So we can generalize our idea of an algebra even more. It's just some type `τ` with a function `f τ → τ` for some functor `f`. In fact, we could write this out as a typeclass:

    class Functor f ⇒ Algebra f τ where
      op ∷ f τ → τ
    

This is often called an "F-algebra" because it's determined by the functor `F`. If we could partially apply typeclasses, we could define something like `class Monoid = Algebra MonoidArgument`.

Coalgebras
==========

Now, hopefully you have a good grasp of what an algebra is and how it's just a generalization of normal algebraic structures. So what is an F-coalgebra? Well, the co implies that it's the "dual" of an algebra—that is, we take an algebra and flip some arrows. I only see one arrow in the above definition, so I'll just flip that:

    class Functor f ⇒ CoAlgebra f τ where
      coop ∷ τ → f τ
    

And that's all it is! Now, this conclusion may seem a little flippant (heh). It tells you _what_ a coalgebra is, but does not really give any insight on how it's useful or why we care. I'll get to that in a bit, once I find or come up with a good example or two :P.

Classes and Objects
-------------------

After reading around a bit, I think I have a good idea of how to use coalgebras to represent classes and objects. We have a type `C` that contains all the possible internal states of objects in the class; the class itself is a coalgebra over `C` which specifies the methods and properties of the objects.

As shown in the algebra example, if we have a bunch of functions like `a → τ` and `b → τ` for any `a, b,…`, we can combine them all into a single function using `Either`, a sum type. The dual "notion" would be combining a bunch of functions of type `τ → a`, `τ → b` and so on. We can do this using the dual of a sum type—a product type. So given the two functions above (called `f` and `g`), we can create a single one like so:

    both ∷ τ → (a, b)
    both x = (f x, g x)
    

The type `(a, a)` is a functor in the straightforward way, so it certainly fits with our notion of an F-coalgebra. This particular trick lets us package up a bunch of different functions—or, for OOP, methods—into a single function of type `τ → f τ`.

The elements of our type `C` represent the _internal_ state of the object. If the object has some readable properties, they have to be able to depend on the state. The most obvious way to do this is to make them a function of `C`. So if we want a length property (e.g. `object.length`), we would have a function `C → Int`.

We want methods that can take an argument and modify state. To do this, we need to take all the arguments and produce a new `C`. Let's imagine a `setPosition` method which takes an `x` and a `y` coordinate: `object.setPosition(1, 2)`. It would look like this: `C → ((Int, Int) → C)`.

The important pattern here is that the "methods" and "properties" of the object take the object itself as their first argument. This is just like the `self` parameter in Python and like the implicit `this` of many other languages. A coalgebra essentially just encapsulates the behavior of taking a `self` parameter: that's what the first `C` in `C → F C` is.

So let's put it all together. Let's imagine a class with a `position` property, a `name` property and `setPosition` function:

    class C
      private
        x, y  : Int
        _name : String
      public
        name        : String
        position    : (Int, Int)
        setPosition : (Int, Int) → C
    

We need two parts to represent this class. First, we need to represent the internal state of the object; in this case it just holds two `Int`s and a `String`. (This is our type `C`.) Then we need to come up with the coalgebra representing the class.

    data C = Obj { x, y  ∷ Int
                 , _name ∷ String }
    

We have two properties to write. They're pretty trivial:

    position ∷ C → (Int, Int)
    position self = (x self, y self)
    
    name ∷ C → String
    name self = _name self
    

Now we just need to be able to update the position:

    setPosition ∷ C → (Int, Int) → C
    setPosition self (newX, newY) = self { x = newX, y = newY }
    

This is just like a Python class with its explicit `self` variables. Now that we have a bunch of `self →` functions, we need to combine them into a single function for the coalgebra. We can do this with a simple tuple:

    coop ∷ C → ((Int, Int), String, (Int, Int) → C)
    coop self = (position self, name self, setPosition self)
    

The type `((Int, Int), String, (Int, Int) → c)`—for _any_ `c`—is a functor, so `coop` does have the form we want: `Functor f ⇒ C → f C`.

Given this, `C` along with `coop` form a coalgebra which specifies the class I gave above. You can see how we can use this same technique to specify any number of methods and properties for our objects to have.

This lets us use coalgebraic reasoning to deal with classes. For example, we can bring in the notion of an "F-coalgebra homomorphism" to represent transformations between classes. This is a scary sounding term that just means a transformation between coalgebras that preserves structure. This makes it much easier to think about mapping classes onto other classes.

In short, an F-coalgebra represents a class by having a bunch of properties and methods that all depend on a `self` parameter containing each object's internal state.

Other Categories
================

So far, we've talked about algebras and coalgebras as Haskell types. An algebra is just a type `τ` with a function `f τ → τ` and a coalgebra is just a type `τ` with a function `τ → f τ`.

However, nothing really ties these ideas to Haskell _per se_. In fact, they're usually introduced in terms of sets and mathematical functions rather than types and Haskell functions. Indeed,we can generalize these concepts to _any_ categories!

We can define an F-algebra for some category `C`. First, we need a functor `F : C → C`—that is, an _endofunctor_. (All Haskell `Functor`s are actually endofunctors from `Hask → Hask`.) Then, an algebra is just an object `A` from `C` with a morphism `F A → A`. A coalgebra is the same except with `A → F A`.

What do we gain by considering other categories? Well, we can use the same ideas in different contexts. Like monads. In Haskell, a monad is some type `M ∷ ★ → ★` with three operations:

    map      ∷ (α → β) → (M α → M β)
    return   ∷ α → M α
    join     ∷ M (M α) → M α
    

The `map` function is just a proof of the fact that `M` is a `Functor`. So we can say that a monad is just a functor with _two_ operations: `return` and `join`.

Functors form a category themselves, with morphisms between them being so-called "natural transformations". A natural transformation is just a way to transform one functor into another while preserving its structure. [Here's](http://lukepalmer.wordpress.com/2008/04/28/whats-a-natural-transformation/) a nice article helping explain the idea. It talks about `concat`, which is just `join` for lists.

With Haskell functors, the composition of two functors is a functor itself. In pseudocode, we could write this:

    instance (Functor f, Functor g) ⇒ Functor (f ∘ g) where
      fmap fun x = fmap (fmap fun) x
    

This helps us think about `join` as a mapping from `f ∘ f → f`. The type of `join` is `∀α. f (f α) → f α`. Intuitively, we can see how a function valid for _all_ types `α` can be thought of as a transformation of `f`.

`return` is a similar transformation. Its type is `∀α. α → f α`. This looks different—the first `α` is not "in" a functor! Happily, we can fix this by adding an identity functor there: `∀α. Identity α → f α`. So `return` is a transformation `Identity → f`.

Now we can think about a monad as just an algebra based around some functor `f` with operations `f ∘ f → f` and `Identity → f`. Doesn't this look familiar? It's very similar to a monoid, which was just some type `τ` with operations `τ × τ → τ` and `() → τ`.

So a monad is just like a monoid, except instead of having a type we have a functor. It's the same sort of algebra, just in a different category. (This is where the phrase "A monad is just a monoid in the category of endofunctors" comes from as far as I know.)

Now, we have these two operations: `f ∘ f → f` and `Identity → f`. To get the corresponding coalgebra, we just flip the arrows. This gives us two new operations: `f → f ∘ f` and `f → Identity`. We can turn them into Haskell types by adding type variables as above, giving us `∀α. f α → f (f α)` and `∀α. f α → α`. This looks just like the definition of a comonad:

    class Functor f ⇒ Comonad f where
      coreturn ∷ f α → α
      cojoin   ∷ f α → f (f α)
    

So a comonad is then a _coalgebra_ in a category of endofunctors.
