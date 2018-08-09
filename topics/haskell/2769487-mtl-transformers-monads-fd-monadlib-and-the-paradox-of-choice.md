
# mtl, transformers, monads-fd, monadLib, and the paradox of choice

## Question
        
Hackage has several packages for monad transformers:

*   [mtl](http://hackage.haskell.org/package/mtl): Monad transformer library
*   [transformers](http://hackage.haskell.org/package/transformers): Concrete functor and monad transformers
*   [monads-fd](http://hackage.haskell.org/package/monads-fd): Monad classes, using functional dependencies
*   [monads-tf](http://hackage.haskell.org/package/monads-tf): Monad classes, using type families
*   [monadLib](http://hackage.haskell.org/package/monadLib): A collection of monad transformers.
*   [mtl-tf](http://hackage.haskell.org/package/mtl-tf): Monad transformer library using type families.
*   [mmtl](http://hackage.haskell.org/package/mmtl): Modular Monad transformer library
*   [mtlx](http://hackage.haskell.org/package/mtlx): Monad transformer library with type indexes, providing 'free' copies.
*   [compose-trans](http://hackage.haskell.org/package/compose-trans): Composable monad transformers

(and maybe I missed some)

Which one shall we use?

mtl is the one in the Haskell Platform, but I keep hearing on reddit that it's uncool.

But what's bad about choice anyway, isn't it just a good thing?

Well, I saw how for example the authors of data-accessor had to make all these to cater to just the popular choices:

*   data-accessor-monadLib library: Accessor functions for monadLib's monads
*   data-accessor-monads-fd library: Use Accessor to access state in monads-fd State monad class
*   data-accessor-monads-tf library: Use Accessor to access state in monads-tf State monad type family
*   data-accessor-mtl library: Use Accessor to access state in mtl State monad class
*   data-accessor-transformers library: Use Accessor to access state in transformers State monad

I imagine that if this goes on and for example several competing Arrow packages evolve, we might see something like: spoonklink-arrows-transformers, spoonklink-arrows-monadLib, spoonklink-tfArrows-transformers, spoonklink-tfArrows-monadLib, ...

And then I worry that if spoonklink gets forked, Hackage will run out of disk space. :)

Questions:

*   Why are there so many monad transformer packages?
*   Why is mtl \[considered\] uncool?
*   What are the key differences?
*   Most of these seemingly competing packages were written by Andy Gill and are maintained by Ross Paterson. Does this mean that these packages are not competing but rather work together in some way? And do Andy and Ross consider any of their own packages as obsolete?
*   Which one should you and I use?

## Answer
        
A bunch of them are almost completely equivalent:

*   `mtl` uses GHC extensions, but `transformers` is Haskell 98.
*   `monads-fd` and `monads-tf` are add-ons to `transformers`, using functional dependencies and type families respectively, both providing the functionality in `mtl` that's missing from `transformers`.
*   `mtl-tf` is `mtl` reimplemented using type families.

So essentially, `mtl` == `transformers` \+\+ `monads-fd`, `mtl-tf` == `transformers` \+\+ `monads-tf`. The improved portability and modularity of `transformers` and its associated packages is why `mtl` is uncool these days, I think.

`mmtl` and `mtlx` both seem to be similar to and/or based on `mtl`, with API differences and extra features.

`MonadLib` seems to have a rather different take on matters, but I'm not familiar with it directly. Also seems to use a lot of GHC extensions, more than the others.

At a glance `compose-trans` seems to be more like metaprogramming stuff for creating monad transformers. It claims to be compatible with `Control.Monad.Trans` which... I guess means `mtl`?

At any rate, I'd suggest the following decision algorithm:

*   Do you need standard monads for a new project? Use `transformers` & co., help us lay `mtl` to rest.
*   Are you already using `mtl` in a large project? `transformers` isn't completely compatible, but no one will kill you for not switching.
*   Does one of the other packages provide unusual functionality that you need? Might as well use it rather than rolling your own.
*   Still unsatisfied? Throw them all out, download [`category-extras`](https://hackage.haskell.org/package/category-extras), and solve all the world's problems with a page and a half of incomprehensible abstract nonsense breathtakingly generic code.
