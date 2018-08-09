
# Haskell&apos;s algebraic data types

## Question
        
I'm trying to fully understand all of Haskell's concepts.

In what ways are algebraic data types similar to generic types, e.g., in C# and Java? And how are they different? What's so algebraic about them anyway?

I'm familiar with universal algebra and its rings and fields, but I only have a vague idea of how Haskell's types work.

## Answer
        
"Algebraic Data Types" in Haskell support _full parametric polymorphism_, which is the more technically correct name for generics, as a simple example the list data type:

     data List a = Cons a (List a) | Nil
    

Is equivalent (as much as is possible, and ignoring non-strict evaluation, etc) to

     class List<a> {
         class Cons : List<a> {
             a head;
             List<a> tail;
         }
         class Nil : List<a> {}
     }
    

Of course Haskell's type system allows more ... interesting use of type parameters but this is just a simple example. With regards to the "Algebraic Type" name, i've honestly never been entirely sure of the exact reason for them being named that, but have assumed that it's due the mathematical underpinnings of the type system. I _believe_ that the reason boils down to the theoretical definition of an ADT being the "product of a set of constructors", however it's been a couple of years since i escaped university so i can no longer remember the specifics.

\[Edit: Thanks to Chris Conway for pointing out my foolish error, ADT are of course sum types, the constructors providing the product/tuple of fields\]
