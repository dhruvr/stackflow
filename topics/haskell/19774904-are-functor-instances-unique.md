
# Are Functor instances unique?

## Question
        
I was wondering to what extent `Functor` instances in Haskell are determined (uniquely) by the functor laws.

Since `ghc` can derive `Functor` instances for at least "run-of-the-mill" data types, it seems that they must be unique at least in a wide variety of cases.

For convenience, the `Functor` definition and functor laws are:

    class Functor f where
      fmap :: (a -> b) -> f a -> f b
    
    fmap id = id
    fmap (g . h) = (fmap g) . (fmap h)
    

Questions:

*   Can one derive the definition of `map` starting from the assumption that it is a `Functor` instance for `data List a = Nil | Cons a (List a)`? If so, what assumptions have to be made in order to do this?
    
*   Are there any Haskell data types which have more than one `Functor` instances which satisfy the functor laws?
    
*   When can `ghc` derive a `functor` instance and when can't it?
    
*   Does all of this depend how we define equality? The `Functor` laws are expressed in terms of an equality of values, yet we don't require `Functors` to have `Eq` instances. So is there some choice here?
    

Regarding equality, there is certainly a notion of what I call "constructor equality" which allows us to reason that `[a,a,a]` is "equal" to `[a,a,a]` for any value of `a` of any type even if `a` does not have `(==)` defined for it. All other (useful) notions of equality are probably coarser that this equivalence relationship. But I suspect that the equality in the `Functor` laws are more of an "reasoning equality" relationship and can be application specific. Any thoughts on this?

## Answer
        
See Brent Yorgey's [Typeclassopedia](http://www.haskell.org/haskellwiki/Typeclassopedia):

> Unlike some other type classes we will encounter, a given type has at most one valid instance of Functor. This [can be proven](http://article.gmane.org/gmane.comp.lang.haskell.libraries/15384) via the [free theorem](http://homepages.inf.ed.ac.uk/wadler/topics/parametricity.html#free) for the type of fmap. In fact, [GHC can automatically derive](http://byorgey.wordpress.com/2010/03/03/deriving-pleasure-from-ghc-6-12-1/) Functor instances for many data types.
