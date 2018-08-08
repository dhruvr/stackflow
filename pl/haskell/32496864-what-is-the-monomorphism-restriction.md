
# What is the monomorphism restriction?

## Question
      
I'm puzzled by how the haskell compiler sometimes infers types that are less polymorphic than what I'd expect, for example when using point-free definitions.

It seems like the issue is the "monomorphism restriction", which is on by default on older versions of the compiler.

Consider the following haskell program:

    {-# LANGUAGE MonomorphismRestriction #-}
    
    import Data.List(sortBy)
    
    plus = (+)
    plus' x = (+ x)
    
    sort = sortBy compare
    
    main = do
      print $ plus' 1.0 2.0
      print $ plus 1.0 2.0
      print $ sort [3, 1, 2]
    

If I compile this with `ghc` I obtain no erros and the output of the executable is:

    3.0
    3.0
    [1,2,3]
    

If I change the `main` body to:

    main = do
      print $ plus' 1.0 2.0
      print $ plus (1 :: Int) 2
      print $ sort [3, 1, 2]
    

I get no compile time errors and the output becomes:

    3.0
    3
    [1,2,3]
    

as expected. However if I try to change it to:

    main = do
      print $ plus' 1.0 2.0
      print $ plus (1 :: Int) 2
      print $ plus 1.0 2.0
      print $ sort [3, 1, 2]
    

I get a type error:

    test.hs:13:16:
        No instance for (Fractional Int) arising from the literal ‘1.0’
        In the first argument of ‘plus’, namely ‘1.0’
        In the second argument of ‘($)’, namely ‘plus 1.0 2.0’
        In a stmt of a 'do' block: print $ plus 1.0 2.0
    

The same happens when trying to call `sort` twice with different types:

    main = do
      print $ plus' 1.0 2.0
      print $ plus 1.0 2.0
      print $ sort [3, 1, 2]
      print $ sort "cba"
    

produces the following error:

    test.hs:14:17:
        No instance for (Num Char) arising from the literal ‘3’
        In the expression: 3
        In the first argument of ‘sort’, namely ‘[3, 1, 2]’
        In the second argument of ‘($)’, namely ‘sort [3, 1, 2]’
    

*   Why does `ghc` suddenly think that `plus` isn't polymorphic and requires an `Int` argument? The only reference to `Int` is in _an application_ of `plus`, how can that matter when the definition is clearly polymorphic?
*   Why does `ghc` suddenly think that `sort` requires a `Num Char` instance?

Moreover if I try to place the function definitions into their own module, as in:

    {-# LANGUAGE MonomorphismRestriction #-}
    
    module TestMono where
    
    import Data.List(sortBy)
    
    plus = (+)
    plus' x = (+ x)
    
    sort = sortBy compare
    

I get the following error when compiling:

    TestMono.hs:10:15:
        No instance for (Ord a0) arising from a use of ‘compare’
        The type variable ‘a0’ is ambiguous
        Relevant bindings include
          sort :: [a0] -> [a0] (bound at TestMono.hs:10:1)
        Note: there are several potential instances:
          instance Integral a => Ord (GHC.Real.Ratio a)
            -- Defined in ‘GHC.Real’
          instance Ord () -- Defined in ‘GHC.Classes’
          instance (Ord a, Ord b) => Ord (a, b) -- Defined in ‘GHC.Classes’
          ...plus 23 others
        In the first argument of ‘sortBy’, namely ‘compare’
        In the expression: sortBy compare
        In an equation for ‘sort’: sort = sortBy compare
    

*   Why isn't `ghc` able to use the polymorphic type `Ord a => [a] -> [a]` for `sort`?
*   And why does `ghc` treat `plus` and `plus'` differently? `plus` should have the polymorphic type `Num a => a -> a -> a` and I don't really see how this is different from the type of `sort` and yet only `sort` raises an error.

Last thing: if I comment the definition of `sort` the file compiles. However if I try to load it into `ghci` and check the types I get:

    *TestMono> :t plus
    plus :: Integer -> Integer -> Integer
    *TestMono> :t plus'
    plus' :: Num a => a -> a -> a
    

Why isn't the type for `plus` polymorphic?

* * *

This is the canonical question about monomorphism restriction in Haskell as discussed in [the meta question](https://meta.stackoverflow.com/questions/294053/can-we-provide-a-canonical-questionanswer-for-haskells-monomorphism-restrictio).
## Answer
      
What is the monomorphism restriction?
=====================================

The [monomorphism restriction](https://wiki.haskell.org/Monomorphism_restriction) as stated by the Haskell wiki is:

> a counter-intuitive rule in Haskell type inference. If you forget to provide a type signature, sometimes this rule will fill the free type variables with specific types using "type defaulting" rules.

What this means is that, _in some circumstances_, if your type is ambiguous (i.e. polymorphic) the compiler will choose to _instantiate_ that type to something not ambiguous.

How do I fix it?
================

First of all you can always explicitly provide a type signature and this will avoid the triggering of the restriction:

    plus :: Num a => a -> a -> a
    plus = (+)    -- Okay!
    
    -- Runs as:
    Prelude> plus 1.0 1
    2.0
    

Alternatively, if you are defining a function, you can _avoid_ [point-free style](https://wiki.haskell.org/Pointfree), and for example write:

    plus x y = x + y
    

Turning it off
--------------

It is possible to simply turn off the restriction so that you don't have to do anything to your code to fix it. The behaviour is controlled by two extensions: `MonomorphismRestriction` will enable it (which is the default) while `NoMonomorphismRestriction` will disable it.

You can put the following line at the very top of your file:

    {-# LANGUAGE NoMonomorphismRestriction #-}
    

If you are using GHCi you can enable the extension using the `:set` command:

    Prelude> :set -XNoMonomorphismRestriction
    

You can also tell `ghc` to enable the extension from the command line:

    ghc ... -XNoMonomorphismRestriction
    

**Note:** You should really prefer the first option over choosing extension via command-line options.

Refer to [GHC's page](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/other-type-extensions.html#monomorphism) for an explanation of this and other extensions.

A complete explanation
======================

I'll try to summarize below everything you need to know to understand what the monomorphism restriction is, why it was introduced and how it behaves.

### An example

Take the following trivial definition:

    plus = (+)
    

you'd think to be able to replace every occurrence of `+` with `plus`. In particular since `(+) :: Num a => a -> a -> a` you'd expect to also have `plus :: Num a => a -> a -> a`.

Unfortunately this is not the case. For example in we try the following in GHCi:

    Prelude> let plus = (+)
    Prelude> plus 1.0 1
    

We get the following output:

    <interactive>:4:6:
        No instance for (Fractional Integer) arising from the literal ‘1.0’
        In the first argument of ‘plus’, namely ‘1.0’
        In the expression: plus 1.0 1
        In an equation for ‘it’: it = plus 1.0 1
    

You may need to `:set -XMonomorphismRestriction` in newer GHCi versions.

And in fact we can see that the type of `plus` is not what we would expect:

    Prelude> :t plus
    plus :: Integer -> Integer -> Integer
    

What happened is that the compiler saw that `plus` had type `Num a => a -> a -> a`, a polymorphic type. Moreover it happens that the above definition falls under the rules that I'll explain later and so he decided to make the type monomorphic by _defaulting_ the type variable `a`. The default is `Integer` as we can see.

Note that if you try to _compile_ the above code using `ghc` you won't get any errors. This is due to how `ghci` handles (and _must_ handle) the interactive definitions. Basically every statement entered in `ghci` must be _completely_ type checked before the following is considered; in other words it's as if every statement was in a separate _module_. Later I'll explain why this matter.

### Some other example

Consider the following definitions:

    f1 x = show x
    
    f2 = \x -> show x
    
    f3 :: (Show a) => a -> String
    f3 = \x -> show x
    
    f4 = show
    
    f5 :: (Show a) => a -> String
    f5 = show
    

We'd expect all these functions to behave in the same way and have the same type, i.e. the type of `show`: `Show a => a -> String`.

Yet when compiling the above definitions we obtain the following errors:

    test.hs:3:12:
        No instance for (Show a1) arising from a use of ‘show’
        The type variable ‘a1’ is ambiguous
        Relevant bindings include
          x :: a1 (bound at blah.hs:3:7)
          f2 :: a1 -> String (bound at blah.hs:3:1)
        Note: there are several potential instances:
          instance Show Double -- Defined in ‘GHC.Float’
          instance Show Float -- Defined in ‘GHC.Float’
          instance (Integral a, Show a) => Show (GHC.Real.Ratio a)
            -- Defined in ‘GHC.Real’
          ...plus 24 others
        In the expression: show x
        In the expression: \ x -> show x
        In an equation for ‘f2’: f2 = \ x -> show x
    
    test.hs:8:6:
        No instance for (Show a0) arising from a use of ‘show’
        The type variable ‘a0’ is ambiguous
        Relevant bindings include f4 :: a0 -> String (bound at blah.hs:8:1)
        Note: there are several potential instances:
          instance Show Double -- Defined in ‘GHC.Float’
          instance Show Float -- Defined in ‘GHC.Float’
          instance (Integral a, Show a) => Show (GHC.Real.Ratio a)
            -- Defined in ‘GHC.Real’
          ...plus 24 others
        In the expression: show
        In an equation for ‘f4’: f4 = show
    

So `f2` and `f4` don't compile. Moreover when trying to define these function in GHCi we get _no errors_, but the type for `f2` and `f4` is `() -> String`!

Monomorphism restriction is what makes `f2` and `f4` require a monomorphic type, and the different behaviour bewteen `ghc` and `ghci` is due to different _defaulting rules_.

When does it happen?
====================

In Haskell, as defined by the [report](https://www.haskell.org/onlinereport/haskell2010/), there are _two_ distinct type of [_bindings_](https://www.haskell.org/onlinereport/haskell2010/haskellch4.html#x10-830004.4.3). Function bindings and pattern bindings. A function binding is nothing else than a definition of a function:

    f x = x + 1
    

Note that their syntax is:

    <identifier> arg1 arg2 ... argn = expr
    

Modulo guards and `where` declarations. But they don't really matter.

where there must be _at least one argument_.

A pattern binding is a declaration of the form:

    <pattern> = expr
    

Again, modulo guards.

Note that _variables are patterns_, so the binding:

    plus = (+)
    

is a _pattern_ binding. It's binding the pattern `plus` (a variable) to the expression `(+)`.

When a pattern binding consists of only a variable name it's called a _simple_ pattern binding.

**The monomorphism restriction applies to simple pattern bindings!**

Well, formally we should say that:

> _A declaration group_ is a minimal set of mutually dependent bindings.

Section 4.5.1 of the [report](https://www.haskell.org/onlinereport/haskell2010/).

And then (Section 4.5.5 of the [report](https://www.haskell.org/onlinereport/haskell2010/)):

> a given declaration group is _unrestricted_ if and only if:
> 
> 1.  every variable in the group is bound by a function binding (e.g. `f x = x`) or a simple pattern binding (e.g. `plus = (+)`; Section 4.4.3.2 ), and
>     
> 2.  an explicit type signature is given for every variable in the group that is bound by simple pattern binding. (e.g. `plus :: Num a => a -> a -> a; plus = (+)`).
>     

Examples added by me.

So a _restricted_ declaration group is a group where, either there are _non-simple_ pattern bindings (e.g. `(x:xs) = f something` or `(f, g) = ((+), (-))`) or there is some simple pattern binding without a type signature (as in `plus = (+)`).

_The monomorphism restriction affects **restricted** declaration groups._

Most of the time you don't define mutual recursive functions and hence a declaration group becomes just _a_ binding.

What does it do?
================

The monomorphism restriction is described by two rules in Section 4.5.5 of the [report](https://www.haskell.org/onlinereport/haskell2010/).

First rule
----------

> The usual Hindley-Milner restriction on polymorphism is that only type variables that do not occur free in the environment may be generalized. In addition, **the constrained type variables of a restricted declaration group may not be generalized in the generalization step for that group.** (Recall that a type variable is constrained if it must belong to some type class; see Section 4.5.2 .)

The highlighted part is what the monomorphism restriction introduces. It says that _if_ the type is polymorphic (i.e. it contain some type variable) _and_ that type variable is constrained (i.e. it has a class constraint on it: e.g. the type `Num a => a -> a -> a` is polymorphic because it contains `a` and also contrained because the `a` has the constraint `Num` over it.) _then_ it cannot be generalized.

In simple words _not generalizing_ means that the **uses** of the function `plus` may change its type.

If you had the definitions:

    plus = (+)
    
    x :: Integer
    x = plus 1 2
    
    y :: Double
    y = plus 1.0 2
    

then you'd get a type error. Because when the compiler sees that `plus` is called over an `Integer` in the declaration of `x` it will unify the type variable `a` with `Integer` and hence the type of `plus` becomes:

    Integer -> Integer -> Integer
    

but then, when it will type check the definition of `y`, it will see that `plus` is applied to a `Double` argument, and the types don't match.

Note that you can still use `plus` without getting an error:

    plus = (+)
    x = plus 1.0 2
    

In this case the type of `plus` is first inferred to be `Num a => a -> a -> a` but then its use in the definition of `x`, where `1.0` requires a `Fractional` constraint, will change it to `Fractional a => a -> a -> a`.

### Rationale

The report says:

> Rule 1 is required for two reasons, both of which are fairly subtle.
> 
> *   Rule 1 prevents computations from being unexpectedly repeated. For example, `genericLength` is a standard function (in library `Data.List`) whose type is given by
>     
>         genericLength :: Num a => [b] -> a
>         
>     
>     Now consider the following expression:
>     
>         let len = genericLength xs
>         in (len, len)
>         
>     
>     It looks as if `len` should be computed only once, but **without Rule 1 it might be computed twice, once at each of two different overloadings.** If the programmer does actually wish the computation to be repeated, an explicit type signature may be added:
>     
>         let len :: Num a => a
>             len = genericLength xs
>         in (len, len)
>         
>     

For this point the example from the [wiki](https://wiki.haskell.org/Monomorphism_restriction) is, I believe, clearer. Consider the function:

    f xs = (len, len)
      where
        len = genericLength xs
    

If `len` was polymorphic the type of `f` would be:

    f :: Num a, Num b => [c] -> (a, b)
    

So the two elements of the tuple `(len, len)` could actually be _different_ values! But this means that the computation done by `genericLength` _must_ be repeated to obtain the two different values.

The rationale here is: the code contains _one_ function call, but not introducing this rule could produce _two_ hidden function calls, which is counter intuitive.

With the monomorphism restriction the type of `f` becomes:

    f :: Num a => [b] -> (a, a)
    

In this way there is no need to perform the computation multiple times.

> *   Rule 1 prevents ambiguity. For example, consider the declaration group
>     
>     \[(n,s)\] = reads t
>     
>     Recall that `reads` is a standard function whose type is given by the signature
>     
>     reads :: (Read a) => String -> \[(a,String)\]
>     
>     Without Rule 1, `n` would be assigned the type `∀ a. Read a ⇒ a` and `s` the type `∀ a. Read a ⇒ String`. The latter is an invalid type, because it is inherently ambiguous. It is not possible to determine at what overloading to use `s`, nor can this be solved by adding a type signature for `s`. Hence, when non-simple pattern bindings are used (Section 4.4.3.2 ), the types inferred are always monomorphic in their constrained type variables, irrespective of whether a type signature is provided. In this case, both `n` and `s` are monomorphic in `a`.
>     

Well, I believe this example is self-explanatory. There are situations when not applying the rule results in type ambiguity.

If you disable the extension as suggest above you _will_ get a type error when trying to compile the above declaration. However this isn't really a problem: you already know that when using `read` you have to somehow tell the compiler which type it should try to parse...

Second rule
-----------

> 2.  Any monomorphic type variables that remain when type inference for an entire module is complete, are considered ambiguous, and are resolved to particular types using the defaulting rules (Section 4.3.4 ).

This means that. If you have your usual definition:

    plus = (+)
    

This will have a type `Num a => a -> a -> a` where `a` is a _monomorphic_ type variable due to rule 1 described above. Once the whole module is inferred the compiler will simply choose a type that will replace that `a` according to the defaulting rules.

The final result is: `plus :: Integer -> Integer -> Integer`.

Note that this is done _after_ the whole module is inferred.

This means that if you have the following declarations:

    plus = (+)
    
    x = plus 1.0 2.0
    

inside a module, _before_ type defaulting the type of `plus` will be: `Fractional a => a -> a -> a` (see rule 1 for why this happens). At this point, following the defaulting rules, `a` will be replaced by `Double` and so we will have `plus :: Double -> Double -> Double` and `x :: Double`.

Defaulting
==========

As stated before there exist some _defaulting_ rules, described in [Section 4.3.4 of the Report](https://www.haskell.org/onlinereport/haskell2010/haskellch4.html#x10-790004.3.4), that the inferencer can adopt and that will replace a polymorphic type with a monomorphic one. This happens whenever a type is _ambiguous_.

For example in the expression:

    let x = read "<something>" in show x
    

here the expression is ambiguous because the types for `show` and `read` are:

    show :: Show a => a -> String
    read :: Read a => String -> a
    

So the `x` has type `Read a => a`. But this constraint is satisfied by a lot of types: `Int`, `Double` or `()` for example. Which one to choose? There's nothing that can tell us.

In this case we can resolve the ambiguity by telling the compiler which type we want, adding a type signature:

    let x = read "<something>" :: Int in show x
    

Now the problem is: since Haskell uses the `Num` type class to handle numbers, there are _a lot_ of cases where numerical expressions contain ambiguities.

Consider:

    show 1
    

What should the result be?

As before `1` has type `Num a => a` and there are many type of numbers that could be used. Which one to choose?

Having a compiler error almost every time we use a number isn't a good thing, and hence the defaulting rules were introduced. The rules can be controlled using a `default` declaration. By specifying `default (T1, T2, T3)` we can change how the inferencer defaults the different types.

An ambiguous type variable `v` is defaultable if:

*   `v` appears only in contraints of the kind `C v` were `C` is a class (i.e. if it appears as in: `Monad (m v)` then it is **not** defaultable).
*   at least one of these classes is `Num` or a subclass of `Num`.
*   all of these classes are defined in the Prelude or a standard library.

A defaultable type variable is replaced by the _first_ type in the `default` list that is an instance of all the ambiguous variable’s classes.

The default `default` declaration is `default (Integer, Double)`.

For example:

    plus = (+)
    minus = (-)
    
    x = plus 1.0 1
    y = minus 2 1
    

The types inferred would be:

    plus :: Fractional a => a -> a -> a
    minus :: Num a => a -> a -> a
    

which, by defaulting rules, become:

    plus :: Double -> Double -> Double
    minus :: Integer -> Integer -> Integer
    

Note that this explains why in the example in the question only the `sort` definition raises an error. The type `Ord a => [a] -> [a]` cannot be defaulted because `Ord` isn't a numeric class.

### Extended defaulting

Note that GHCi comes with [_extended_ defaulting rules](https://downloads.haskell.org/~ghc/7.8.4/docs/html/users_guide/interactive-evaluation.html#extended-default-rules) (or [herefor GHC8](https://downloads.haskell.org/~ghc/8.0.2/docs/html/users_guide/ghci.html#type-defaulting-in-ghci)), which can be enabled in files as well using the `ExtendedDefaultRules` extensions.

The defaultable type variables need not _only_ appear in contraints where all the classes are standard and there must be at least one class that is among `Eq`, `Ord`, `Show` or `Num` and its subclasses.

Moreover the default `default` declaration is `default ((), Integer, Double)`.

This may produce odd results. Taking the example from the question:

    Prelude> :set -XMonomorphismRestriction
    Prelude> import Data.List(sortBy)
    Prelude Data.List> let sort = sortBy compare
    Prelude Data.List> :t sort
    sort :: [()] -> [()]
    

in ghci we don't get a type error but the `Ord a` constraints results in a default of `()` which is pretty much useless.

### Useful links

There are _a lot_ of resources and discussions about the monomorphism restriction.

Here are some links that I find useful and that may help you understand or deep further into the topic:

*   Haskell's wiki page: [Monomorphism Restriction](https://wiki.haskell.org/Monomorphism_restriction)
*   The [report](https://www.haskell.org/onlinereport/haskell2010/)
*   An accessible and nice [blog post](http://lambda.jstolarek.com/2012/05/towards-understanding-haskells-monomorphism-restriction/)
*   Sections 6.2 and 6.3 of [A History Of Haskell: Being Lazy With Class](http://research.microsoft.com/en-us/um/people/simonpj/papers/history-of-haskell/history.pdf) deals with the monomorphism restriction and type defaulting
    