
# What exactly is the kind &#x201C;*&#x201D; in Haskell?

## Question
        
In Haskell, (value-level) expressions are classified into _types_, which can be notated with `::` like so: `3 :: Int`, `"Hello" :: String`, `(+ 1) :: Num a => a -> a`. Similarly, types are classified into _kinds_. In GHCi, you can inspect the kind of a type expression using the command `:kind` or `:k`:

    > :k Int
    Int :: *
    > :k Maybe
    Maybe :: * -> *
    > :k Either
    Either :: * -> * -> *
    > :k Num
    Num :: * -> Constraint
    > :k Monad
    Monad :: (* -> *) -> Constraint
    

There are definitions floating around that `*` is the kind of "concrete types" or "values" or "runtime values." See, for example, [Learn You A Haskell](http://learnyouahaskell.com/making-our-own-types-and-typeclasses#kinds-and-some-type-foo). How true is that? We've had [a few](https://stackoverflow.com/questions/24153959/) [questions](https://stackoverflow.com/questions/9892814/) [about kinds](https://stackoverflow.com/questions/16359118/) that address the topic in passing, but it'd be nice to have a canonical and precise explanation of `*`.

What _exactly_ does `*` mean? And how does it relate to other more complex kinds?

Also, do the `DataKinds` or `PolyKinds` extensions change the answer?

## Answer
        
First off, `*` is not a wildcard! It's also typically pronounced "star."

_Bleeding edge note_: There is as of Feb. 2015 [a proposal to simplify GHC's subkind system (in 7.12 or later)](https://ghc.haskell.org/trac/ghc/wiki/NoSubKinds). That page contains a good discussion of the GHC 7.8/7.10 story. Looking forward, GHC may drop the distinction between types and kinds, with `* :: *`. See [Weirich, Hsu, and Eisenberg, System FC with Explicit Kind Equality](http://www.cis.upenn.edu/~eir/papers/2013/fckinds/fckinds.pdf).

The Standard: A description of type expressions.
------------------------------------------------

The Haskell 98 report [defines `*` in this context as](https://www.haskell.org/onlinereport/decls.html#sect4.1.1):

> The symbol `*` represents the kind of all nullary type constructors.

In this context, "nullary" simply means that the constructor takes no parameters. `Either` is binary; it can be applied to two parameters: `Either a b`. `Maybe` is unary; it can be applied to one parameter: `Maybe a`. `Int` is nullary; it can be applied to _no_ parameters.

This definition is a little bit incomplete on its own. An expression containing a fully-applied unary, binary, etc. type constructor also has kind `*`, e.g. `Maybe Int :: *`.

In GHC: Something that contains values?
---------------------------------------

If we poke around the GHC documentation, we get something closer to the "can contain a runtime value" definition. The [GHC Commentary page "Kinds"](https://ghc.haskell.org/trac/ghc/wiki/Commentary/Compiler/Kinds) states that "'`*`' is the kind of boxed values. Things like `Int` and `Maybe Float` have kind `*`." The [GHC user's guide for version 7.4.1](https://downloads.haskell.org/~ghc/7.4.1/docs/html/users_guide/kind-polymorphism-and-promotion.html), on the other hand, stated that `*` is the kind of "lifted types". (That passage wasn't retained when the section was revised for `PolyKinds`.)

Boxed values and lifted types are a bit different. According to the [GHC Commentary page "TypeType"](https://ghc.haskell.org/trac/ghc/wiki/Commentary/Compiler/TypeType#Classifyingtypes),

> A type is unboxed iff its representation is other than a pointer. Unboxed types are also unlifted.
> 
> A type is lifted iff it has bottom as an element. Closures always have lifted types: i.e. any let-bound identifier in Core must have a lifted type. Operationally, a lifted object is one that can be entered. Only lifted types may be unified with a type variable.

So `ByteArray#`, the type of raw blocks of memory, is _boxed_ because it is represented as a pointer, but _unlifted_ because bottom is not an element.

    > undefined :: ByteArray#
    Error: Kind incompatibility when matching types:
       a0 :: *
       ByteArray# :: #
    

Therefore it appears that the old User's Guide definition is more accurate than the GHC Commentary one: **`*` is the kind of _lifted_ types.** (And, conversely, `#` is the kind of _unlifted_ types.)

Note that if types of kind `*` are always lifted, for any type `t :: *` you can construct a "value" of sorts with `undefined :: t` or some other mechanism to create bottom. Therefore even "logically uninhabited" types like `Void` can have a value, i.e. bottom.

So it seems that, yes, `*` represents the kind of types that can contain runtime values, if `undefined` is your idea of a runtime value. (Which isn't a totally crazy idea, I don't think.)

GHC Extensions?
---------------

There are several extensions which liven up the kind system a bit. Some of these are mundane: `KindSignatures` lets us write _kind annotations_, like type annotations.

`ConstraintKinds` adds the kind `Constraint`, which is, roughly, the kind of the left-hand side of `=>`.

`DataKinds` lets us introduce new kinds besides `*` and `#`, just as we can introduce new types with `data`, `newtype`, and `type`.

With `DataKinds` every `data` declaration (terms and conditions may apply) generates a promoted kind declaration. So

     data Bool = True | False
    

introduces the usual value constructor and type name; additionally, it produces a new _kind_, `Bool`, and two types: `True :: Bool` and `False :: Bool`.

`PolyKinds` introduces _kind variables_. This just a way to say "for any kind `k`" just like we say "for any type `t`" at the type level. As regards our friend `*` and whether it still means "types with values", I suppose you could say a type `t :: k` where `k` is a kind variable _could_ contain values, if `k ~ *` or `k ~ #`.
