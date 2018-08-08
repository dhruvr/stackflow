
# Good examples of Not a Functor/Functor/Applicative/Monad?

## Question
      
While explaining to someone what a type class X is I struggle to find good examples of data structures which are exactly X.

So, I request examples for:

*   A type constructor which is not a Functor.
*   A type constructor which is a Functor, but not Applicative.
*   A type constructor which is an Applicative, but is not a Monad.
*   A type constructor which is a Monad.

I think there are plenty examples of Monad everywhere, but a good example of Monad with some relation to previous examples could complete the picture.

I look for examples which would be similar to each other, differing only in aspects important for belonging to the particular type class.

If one could manage to sneak up an example of Arrow somewhere in this hierarchy (is it between Applicative and Monad?), that would be great too!
## Answer
      
**A type constructor which is not a Functor:**

    newtype T a = T (a -> Int)
    

You can make a contravariant functor out of it, but not a (covariant) functor. Try writing `fmap` and you'll fail. Note that the contravariant functor version is reversed:

    fmap      :: Functor f       => (a -> b) -> f a -> f b
    contramap :: Contravariant f => (a -> b) -> f b -> f a
    

**A type constructor which is a functor, but not Applicative:**

I don't have a good example. There is `Const`, but ideally I'd like a concrete non-Monoid and I can't think of any. All types are basically numeric, enumerations, products, sums, or functions when you get down to it. You can see below pigworker and I disagreeing about whether `Data.Void` is a `Monoid`;

    instance Monoid Data.Void where
        mempty = undefined
        mappend _ _ = undefined
        mconcat _ = undefined
    

Since `_|_` is a legal value in Haskell, and in fact the only legal value of `Data.Void`, this meets the Monoid rules. I am unsure what `unsafeCoerce` has to do with it, because your program is no longer guaranteed not to violate Haskell semantics as soon as you use any `unsafe` function.

See the Haskell Wiki for an article on bottom ([link](http://www.haskell.org/haskellwiki/Bottom)) or unsafe functions ([link](http://www.haskell.org/haskellwiki/Unsafe)).

I wonder if it is possible to create such a type constructor using a richer type system, such as Agda or Haskell with various extensions.

**A type constructor which is an Applicative, but not a Monad:**

    newtype T a = T {multidimensional array of a}
    

You can make an Applicative out of it, with something like:

    mkarray [(+10), (+100), id] <*> mkarray [1, 2]
      == mkarray [[11, 101, 1], [12, 102, 2]]
    

But if you make it a monad, you could get a dimension mismatch. I suspect that examples like this are rare in practice.

**A type constructor which is a Monad:**

    []
    

**About Arrows:**

Asking where an Arrow lies on this hierarchy is like asking what kind of shape "red" is. Note the kind mismatch:

    Functor :: * -> *
    Applicative :: * -> *
    Monad :: * -> *
    

but,

    Arrow :: * -> * -> *
    