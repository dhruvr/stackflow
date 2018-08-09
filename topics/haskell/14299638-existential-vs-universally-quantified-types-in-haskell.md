
# Existential vs. Universally quantified types in Haskell

## Question
        
What exactly is the difference between these? I think I understand how existential types work, they are like having a base class in OO without a way to down cast. How are universal types different?

## Answer
        
The terms "universal" and "existential" here come from the similarly-named quantifiers in [predicate logic](http://en.wikipedia.org/wiki/Predicate_logic).

[Universal quantification](http://en.wikipedia.org/wiki/Universal_quantification) is normally written as ∀, which you can read as "for all", and means roughly what it sounds like: in a logical statement resembling "∀x. ..." whatever is in place of the "..." is true for all possible "x" you could choose from whatever set of things is being quantified over.

[Existential quantification](http://en.wikipedia.org/wiki/Existential_quantification) is normally written as ∃, which you can read as "there exists", and means that in a logical statement resembling "∃x. ..." whatever is in place of the "..." is true for some unspecified "x" taken from the set of things being quantified over.

In Haskell, the things being quantified over are types (ignoring certain language extensions, at least), our logical statements are also types, and instead of being "true" we think about "can be implemented".

So, a universally quantified type like `forall a. a -> a` means that, for any possible type "a", we can implement a function whose type is `a -> a`. And indeed we can:

    id :: forall a. a -> a
    id x = x
    

Since `a` is universally quantified we know nothing about it, and therefore cannot inspect the argument in any way. So `id` is the only possible function of that type(1).

In Haskell, universal quantification is the "default"--any type variables in a signature are implicitly universally quantified, which is why the type of `id` is normally written as just `a -> a`. This is also known as [parametric polymorphism](http://en.wikipedia.org/wiki/Parametric_polymorphism), often just called "polymorphism" in Haskell, and in some other languages (e.g., C#) known as "generics".

An _existentially_ quantified type like `exists a. a -> a` means that, for _some particular_ type "a", we can implement a function whose type is `a -> a`. Any function will do, so I'll pick one:

    func :: exists a. a -> a
    func True = False
    func False = True
    

...which is of course the "not" function on booleans. But the catch is that we can't _use_ it as such, because all we know about the type "a" is that it exists. Any information about _which_ type it might be has been discarded, which means we can't apply `func` to any values.

This is not very useful.

So what _can_ we do with `func`? Well, we know that it's a function with the same type for its input and output, so we could compose it with itself, for example. Essentially, the only things you can do with something that has an existential type are the things you can do based on the non-existential parts of the type. Similarly, given something of type `exists a. [a]` we can find its length, or concatenate it to itself, or drop some elements, or anything else we can do to any list.

That last bit brings us back around to universal quantifiers, and the reason why Haskell(2) doesn't have existential types directly (my `exists` above is entirely fictitious, alas): since things with existentially quantified types can only be used with operations that have universally quantified types, we can write the type `exists a. a` as `forall r. (forall a. a -> r) -> r`--in other words, for all result types `r`, given a function that for all types `a` takes an argument of type `a` and returns a value of type `r`, we can get a result of type `r`.

If it's not clear to you why those are nearly equivalent, note that the overall type is not universally quantified for `a`--rather, it takes an argument that itself is universally quantified for `a`, which it can then use with whatever specific type it chooses.

* * *

As an aside, while Haskell doesn't really have a notion of subtyping in the usual sense, we can treat quantifiers as expressing a form of subtyping, with a hierarchy going from universal to concrete to existential. Something of type `forall a. a` could be converted to any other type, so it could be seen as a subtype of everything; on the other hand, any type could be converted to the type `exists a. a`, making that a parent type of everything. Of course, the former is impossible (there are no values of type `forall a. a` except errors) and the latter is useless (you can't do anything with the type `exists a. a`), but the analogy works on paper at least. :\]

Note that the equivalence between an existential type and a universally quantified argument works for the same reason that [variance](http://en.wikipedia.org/wiki/Covariance_and_contravariance_%28computer_science%29) flips for function inputs.

* * *

So, the basic idea is roughly that universally quantified types describe things that work the same for any type, while existential types describe things that work with a specific but unknown type.

* * *

**1:** Well, not quite--only if we ignore functions that cause errors, such as `notId x = undefined`, including functions which never terminate, such as `loopForever x = loopForever x`.

**2:** Well, GHC. Without extensions, Haskell only has the implicit universal quantifiers and no real way of talking about existential types at all.
