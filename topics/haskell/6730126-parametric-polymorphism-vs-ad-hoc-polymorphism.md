
# Parametric polymorphism vs Ad-hoc polymorphism

## Question
        
I would like to understand the key difference between parametric polymorphism such as polymorphism of generic classes/functions in the Java/Scala/C++ languages and "ad-hoc" polymorphism in the Haskell type system. I'm familiar with the first kind of languages, but I have never worked with the Haskell.

More precisely:

1.  How is type inference algorithm e.g. in Java different from the type inference in Haskell?
2.  Please, give me an example of the situation where something can be written in Java/Scala but can not be written in Haskell(according to the modular features of these platforms too), and vice-versa.

Thanks in advance.

## Answer
        
As per the [TAPL](http://www.cis.upenn.edu/~bcpierce/tapl/), §23.2:

> Parametric polymorphism (...), allows a single piece of code to be typed “generically,” using variables in place of actual types, and then instantiated with particular types as needed. Parametric definitions are uniform: all of their instances behave the same. (...)
> 
> Ad-hoc polymorphism, by contrast, allows a polymorphic value to exhibit different behaviors when “viewed” at different types. The most common example of ad-hoc polymorphism is overloading, which associates a single function symbol with many implementations; the compiler (or the runtime system, depending on whether overloading resolution is static or dynamic) chooses an appropriate implementation for each application of the function, based on the types of the arguments.

So, if you consider successive stages of history, non-generic official Java (a.k.a pre-[J2SE 5.0](http://en.wikipedia.org/wiki/J2SE_5.0#J2SE_5.0_.28September_30.2C_2004.29), bef. sept. 2004) had ad-hoc polymorphism - so you could [overload a method](http://download.oracle.com/javase/tutorial/java/javaOO/methods.html) \- but not parametric polymorphism, so you couldn't [write a generic method](http://download.oracle.com/javase/tutorial/extra/generics/methods.html). Afterwards you could do both, of course.

By comparison, since its very beginning [in 1990](http://research.microsoft.com/en-us/um/people/simonpj/papers/history-of-haskell/), Haskell was parametrically polymorphic, meaning you could write:

    swap :: (A; B) -> (B; A)
    swap (x; y) = (y; x)
    

where A and B are type variables can be instantiated to _all_ types, without assumptions.

But there was no preexisting construct giving _ad-hoc_ polymorphism, which intends to let you write functions that apply to _several_, but _not all_ types. Type classes were implemented as a way of achieving this goal.

They let you describe a _class_ (something akin to a Java interface), giving the _type signature_ of the functions you want implemented for your generic type. Then you can register some (and hopefully, _several_) _instances_ matching this class. In the meantime, you can write a generic method such as :

    between :: (Ord a)  a -> a -> a -> Bool
    between x y z = x ≤ y ^ y ≤ z
    

where the `Ord` is the class that defines the function `(_ ≤ _)`. When used, `(between "abc" "d" "ghi")` is _resolved statically_ to select the right _instance_ for strings (rather than e.g. integers) - exactly at the moment when (Java's) method overloading would.

You can do something similar in Java with [bounded wildcards](http://download.oracle.com/javase/tutorial/java/generics/wildcards.html). But the **key difference between Haskell and Java on that front is that only Haskell can do dictionary passing automatically**: in both languages, given two instances of `Ord T`, say `b0` and `b1`, you can build a function `f` that takes those as arguments and produces the instance for the pair type `(b0, b1)`, using, say, the lexicographic order. Say now that you are given `(("hello", 2), ((3, "hi"), 5))`. In Java you have to remember the instances for `string` and `int`, and pass the correct instance (made of four applications of `f`!) in order to apply `between` to that object. Haskell can apply [compositionality](http://en.wikipedia.org/wiki/Compositionality), and figure out how to build the correct instance given just the ground instances and the `f` constructor (this extends to other constructors, of course) .

* * *

Now, as far as _type inference_ goes (and this should probably be a distinct question), for both languages it is _incomplete_, in the sense that you can always write an **un-annotated** program for which the compiler won't be able to determine the type.

1.  for Haskell, this is because it has [impredicative](http://en.wikipedia.org/wiki/Parametric_polymorphism#Higher-ranked_polymorphism) (a.k.a. first-class) polymorphism, for which type inference is undecidable. Note that on that point, Java is limited to first-order polymorphism (something on which Scala expands).
    
2.  for Java, this is because it supports [contravariant subtyping](http://research.microsoft.com/pubs/64041/fool2007.pdf).
    

But those languages mainly differ in the **range of program statements to which type inference applies** in practice, and in the **importance given to the correctness** of the type inference results.

1.  For Haskell, inference applies to all "non-highly polymorphic" terms, and make a serious effort to return sound results based on published extensions of a well-known algorithm:
    
    *   At its core, Haskell's inference is based on [Hindley-Milner](http://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_inference_algorithm#Hindley.E2.80.93Milner_type_inference_algorithm), which gives you complete results as soon as when infering the type of an application, _type variables_ (e.g. the `A` and `B` in the example above) can be only instantiated with _non-polymorphic_ types (I'm simplifying, but this is essentially the ML-style polymorphism you can find in e.g. Ocaml.).
    *   a recent GHC will make sure that a type annotation may be required [only for a let-binding or λ-abstraction that has a non-Damas-Milner type](http://research.microsoft.com/en-us/um/people/simonpj/papers/boxy/).
    *   Haskell has tried to stay relatively close to this inferrable core across even its most hairy extensions (e.g. [GADTs](http://research.microsoft.com/pubs/67443/gadt-pldi.pdf)). At any rate, proposed extensions nearly always come in a paper with a proof of the _correctness_ of the extended type inference .
2.  For Java, type inference applies in a _much more limited fashion_ anyway :
    
    > Prior to the release of Java 5, there was no type inference in Java. According to the Java language culture, **the type of every variable, method, and dynamically allocated object must be explicitly declared by the programmer**. When generics (classes and methods parameterized by type) were introduced in Java 5, **the language retained this requirement for variables, methods, and allocations**. But the introduction of polymorphic methods (parameterized by type) dictated that either (i) the programmer provide the method type arguments at every polymorphic method call site or (ii) the language support the inference of method type arguments. To avoid creating an additional clerical burden for programmers, the designers of Java 5 elected to perform type inference to determine the type arguments **for polymorphic method calls**. ([source](http://portal.acm.org/citation.cfm?id=1449804), emphasis mine)
    
    The [inference algorithm](http://java.sun.com/docs/books/jls/third_edition/html/expressions.html#15.12.2.7) is essentially [that of GJ](https://lamp.epfl.ch/pizza/gj/Documents/gj-oopsla.pdf), but with a [somewhat](http://www.cis.upenn.edu/~stevez/papers/MZ06-buggy.pdf) [kludgy](http://portal.acm.org/citation.cfm?id=1449804) addition of wildcards as an afterthought (Note that I am not up to date on the possible corrections made in J2SE 6.0, though). The large conceptual difference in approach is that Java's inference is _local_, in the sense that the inferred type of an expression depends only on constraints generated from the type system and on the types of its sub-expressions, but not on the context.
    
    Note that the party line regarding the incomplete & sometimes incorrect type inference is relatively laid back. As per [the spec](http://java.sun.com/docs/books/jls/third_edition/html/expressions.html#15.12.2.7):
    
    > Note also that type inference does not affect soundness in any way. If the types inferred are nonsensical, the invocation will yield a type error. The type inference algorithm should be viewed as a heuristic, designed to perfdorm well in practice. If it fails to infer the desired result, explicit type paramneters may be used instead.
