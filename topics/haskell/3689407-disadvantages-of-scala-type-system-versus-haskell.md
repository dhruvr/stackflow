
# Disadvantages of Scala type system versus Haskell?

## Question
        
I have read that Scala's type system is weakened by Java interoperability and therefore cannot perform some of the same powers as Haskell's type system. Is this true? Is the weakness because of type erasure, or am I wrong in every way? Is this difference the reason that Scala has no typeclasses?

## Answer
        
The big difference is that Scala doesn't have Hindley-Milner global type inference and instead uses a form of local type inference, requiring you to specify types for method parameters and the return type for overloaded or recursive functions.

This isn't driven by type erasure or by other requirements of the JVM. All possible difficulties here can be overcome, and have been, just consider Jaskell - [http://docs.codehaus.org/display/JASKELL/Home](http://docs.codehaus.org/display/JASKELL/Home)

H-M inference doesn't work in an object-oriented context. Specifically, when type-polymorphism is used (as opposed to the ad-hoc polymorphism of type classes). This is crucial for strong interop with other Java libraries, and (to a lesser extent) to get the best possible optimisation from the JVM.

It's not really valid to state that either Haskell or Scala has a stronger type system, just that they are different. Both languages are pushing the boundaries for type-based programming in different directions, and each language has unique strengths that are hard to duplicate in the other.
