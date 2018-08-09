
# Object-oriented programming in a purely functional programming context?

## Question
        
Are there any advantages to using object-oriented programming (OOP) in a functional programming (FP) context?

I have been using [F#](http://en.wikipedia.org/wiki/F_Sharp_%28programming_language%29) for some time now, and I noticed that the more my functions are stateless, the less I need to have them as methods of objects. In particular, there are advantages to relying on type inference to have them usable in as wide a number of situations as possible.

This does not preclude the need for namespaces of some form, which is orthogonal to being OOP. Nor is the use of data structures discouraged. In fact, real use of FP languages depend heavily on data structures. If you look at the F# stack implemented in _[F Sharp Programming/Advanced Data Structures](http://en.wikibooks.org/wiki/F_Sharp_Programming/Advanced_Data_Structures)_, you will find that it is not object-oriented.

In my mind, OOP is heavily associated with having methods that act on the state of the object mostly to _mutate_ the object. In a pure FP context that is not needed nor desired.

A practical reason may be to be able to interact with OOP code, in much the same way F# works with [.NET](http://en.wikipedia.org/wiki/.NET_Framework). Other than that however, are there any reasons? And what is the experience in the Haskell world, where programming is more pure FP?

I will appreciate any references to papers or counterfactual real world examples on the issue.

## Answer
        
The disconnect you see is not of FP vs. OOP. It's mostly about immutability and mathematical formalisms vs. mutability and informal approaches.

First, let's dispense with the mutability issue: you can have FP with mutability and OOP with immutability just fine. Even more-functional-than-thou Haskell lets you play with mutable data all you want, you just have to be explicit about what is mutable and the order in which things happen; and efficiency concerns aside, almost any mutable object could construct and return a new, "updated" instance instead of changing its own internal state.

The bigger issue here is mathematical formalisms, in particular heavy use of algebraic data types in a language little removed from lambda calculus. You've tagged this with Haskell and F#, but realize that's only half of the functional programming universe; the Lisp family has a very different, much more freewheeling character compared to ML-style languages. Most OO systems in wide use today are very informal in nature--formalisms do exist for OO but they're not called out explicitly the way FP formalisms are in ML-style languages.

Many of the apparent conflicts simply disappear if you remove the formalism mismatch. Want to build a flexible, dynamic, ad-hoc OO system on top of a Lisp? Go ahead, it'll work just fine. Want to add a formalized, immutable OO system to an ML-style language? No problem, just don't expect it to play nicely with .NET or Java.

* * *

Now, you may be wondering, what _is_ an appropriate formalism for OOP? Well, here's the punch line: In many ways, it's more function-centric than ML-style FP! I'll refer back to [one of my favorite papers](http://lambda-the-ultimate.org/node/3668) for what seems to be the key distinction: structured data like algebraic data types in ML-style languages provide a concrete representation of the data and the ability to define operations on it; objects provide a black-box abstraction over _behavior_ and the ability to easily replace components.

There's a duality here that goes deeper than just FP vs. OOP: It's closely related to what some programming language theorists call [the Expression Problem](http://en.wikipedia.org/wiki/Expression_Problem): With concrete data, you can easily add new operations that work with it, but changing the data's structure is more difficult. With objects you can easily add new data (e.g., new subclasses) but adding new operations is difficult (think adding a new abstract method to a base class with many descendants).

The reason why I say that OOP is more function-centric is that functions themselves represent a form of behavioral abstraction. In fact, you can simulate OO-style structure in something like Haskell by using records holding a bunch of functions as objects, letting the record type be an "interface" or "abstract base class" of sorts, and having functions that create records replace class constructors. So in that sense, OO languages use higher-order functions far, far more often than, say, Haskell would.

For an example of something like this type of design actually put to very nice use in Haskell, read the source for [the graphics-drawingcombinators package](http://hackage.haskell.org/package/graphics-drawingcombinators), in particular the way that it uses an opaque record type containing functions and combines things only in terms of their behavior.

* * *

**EDIT:** A few final things I forgot to mention above.

If OO indeed makes extensive use of higher-order functions, it might at first seem that it should fit very naturally into a functional language such as Haskell. Unfortunately this isn't quite the case. It is true that _objects_ as I described them (cf. the paper mentioned in the LtU link) fit just fine. in fact, the result is a more pure OO style than most OO languages, because "private members" are represented by values hidden by the closure used to construct the "object" and are inaccessible to anything other than the one specific instance itself. You don't get much more private than that!

What doesn't work very well in Haskell is _subtyping_. And, although I think inheritance and subtyping are all too often misused in OO languages, some form of subtyping is quite useful for being able to combine objects in flexible ways. Haskell lacks an inherent notion of subtyping, and hand-rolled replacements tend to be exceedingly clumsy to work with.

As an aside, most OO languages with static type systems make a complete hash of subtyping as well by being too lax with substitutability and not providing proper support for variance in method signatures. In fact, I think the only full-blown OO language that hasn't screwed it up completely, at least that I know of, is Scala (F# seemed to make too many concessions to .NET, though at least I don't think it makes any _new_ mistakes). I have limited experience with many such languages, though, so I could definitely be wrong here.

On a Haskell-specific note, its "type classes" often look tempting to OO programmers, to which I say: Don't go there. Trying to implement OOP that way will only end in tears. Think of type classes as a replacement for overloaded functions/operators, not OOP.
