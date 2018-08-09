
# Monads vs. Arrows

## Question
        
I'm broadly familiar with the concepts of [monads](http://en.wikipedia.org/wiki/Monads_in_functional_programming) and [arrows](http://en.wikipedia.org/wiki/Arrows_in_functional_programming) as used in functional programming. I also understand that they can be used to solve similar kinds of problems.

However, I'm still a bit confused about how to select which one to use in any given situation.

When should I use monads and when should I use arrows?

## Answer
        
There are two excellent papers by Lindley, Wadler & Yallop (discussed at LTU [here](http://lambda-the-ultimate.org/node/2799)).

The most important thing to understand is that there are _more_ things which are arrows than there are things which are monads. Conversely, monads are strictly more powerful than arrows (the [second paper above](http://homepages.inf.ed.ac.uk/wadler/papers/arrows-and-idioms/arrows-and-idioms.pdf) specifies precisely in which fashion).

In particular, monads are arrows equipped with an apply function of type `(a ~> b, a) ~> b`, where `(~>)` is the constructor for a given arrow. Lindley et al. point out that this destroys the meticulous distinction arrows maintain between terms and commands (or, if you prefer, objects and morphisms).

Applicative functors have a wide variety of applications, particularly for things which are best thought of as operations on streams. One can in fact think of arrows as arising from generalizing the notion of a transformer on streams (i.e. introducing a new language for morphisms on objects constructed by a given applicative functor).

In my experience, because monads blur the distinction between objects and morphisms (i.e., if I'm using the words correctly, give rise to a closed cartesian category), then a term in a monad is generally far more necessarily opaque than a term in an arrow or applicative functor (although note that both let you inject arbitrary functions by the [`arr`](http://hackage.haskell.org/packages/archive/base/latest/doc/html/Control-Arrow.html#v:arr) and [`pure`](http://hackage.haskell.org/packages/archive/base/latest/doc/html/Control-Applicative.html#v:pure) methods respectively).

So if something is _not_ given the characteristics of a monad (even though conceptually it forms one), then it is potentially open to greater inspection and optimization. It is also potentially easier to serialize. Hence the use of applicatives and arrows in parsers and circuit modeling.

* * *

The above attempted to be general and descriptive. Below are some of my opinionated rules of thumb.

If you need to model something that looks like state, start with a monad. If you need to model something that looks like global control flow (i.e. exceptions, continuations), start with a monad. If a requirement arises that conflicts with the power and generality of monads (i.e. for which join `(join :: m (m a) -> m a)` is too powerful), then consider chipping away at the power of the thing you're using.

If you need to model streams, and transformations on streams, and particularly streams for which certain characteristics (particularly unlimited views of the past and future) should be opaque, then start with an applicative functor. If you need stronger reasoning about properties of transformations on streams, then think about reaching for an arrow.

Or, very crudely put, applicatives are for the actions of circuits, arrows are for the structures of circuits, and monads are for general-purpose computational effects.

There's of course much more to the story. For applicatives, see [Conal Elliott's work on FRP](http://conal.net/papers/frp.html) in particular. For arrows, see the [HXT XML parser library](http://www.haskell.org/haskellwiki/HXT), the [Yampa FRP project](http://www.haskell.org/haskellwiki/Yampa), the [Haskell on a Horse web framework](http://www.haskell.org/haskellwiki/Web/Frameworks#Haskell_on_a_Horse), Hudak and Liu's classic ["Plugging a Space Leak with an Arrow"](http://haskell.cs.yale.edu/wp-content/uploads/2011/01/leak.pdf) paper, among other things. For monads, see everywhere. And of course take note that just because something is a monad, that doesn't mean that applicative notation might not be clearer and more expressive.
