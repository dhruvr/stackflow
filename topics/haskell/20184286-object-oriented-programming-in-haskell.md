
# Object oriented programming in Haskell

## Question
        
I'm trying to get an understanding of object oriented style programming in Haskell, knowing that things are going to be a bit different due to lack of mutability. I've played around with type classes, but my understanding of them is limited to them as interfaces. So I've coded up a C++ example, which is the standard diamond with a pure base and virtual inheritance. `Bat` inherits `Flying` and `Mammal`, and both `Flying` and `Mammal` inherit `Animal`.

    #include <iostream>
    
    class Animal
    {
    public:
        virtual std::string transport() const = 0;
        virtual std::string type() const = 0;
        std::string describe() const;
    };
    
    std::string Animal::describe() const 
        { return "I am a " + this->transport() + " " + this->type(); }
    
    class Flying : virtual public Animal 
    {
    public:
        virtual std::string transport() const;
    };
    
    std::string Flying::transport() const { return "Flying"; }
    
    class Mammal : virtual public Animal 
    {
    public:
        virtual std::string type() const;
    };
    
    std::string Mammal::type() const { return "Mammal"; }
    
    class Bat : public Flying, public Mammal {};
    
    int main() {
        Bat b;
        std::cout << b.describe() << std::endl;
        return 0;
    }
    

Basically I'm interested in how to translate such a structure into Haskell, basically that would allow me to have a list of `Animal`s, like I could have an array of (smart) pointers to `Animal`s in C++.

## Answer
        
You just don't want to do that, don't even start. OO sure has its merits, but “classic examples” like your C++ one are almost always contrived structures designed to hammer the paradigm into undergraduate students' brains so they won't start complaining about how stupid the languages are they're supposed to use†.

The idea seems basically modelling “real-world objects” by objects in your programming language. Which can be a good approach for actual programming problems, but it only makes sense if you can in fact draw an analogy between how you'd use the real-world object and how the OO objects are handled inside the program.

Which is just ridiculous for such animal examples. If anything, the methods would have to be stuff like “feed”, “milk”, “slaughter”... but “transport” is a misnomer, I'd take that to actually _move_ the animal, which would rather be a method of the environment the animal lives in, and basically makes only sense as part of a visitor pattern.

`describe`, `type` and what you call `transport` are, on the other hand, much simpler. These are basically type-dependent constants or simple pure functions. Only OO paranoia‡ ratifies making them class methods.

Any thing along the lines of this animal stuff, where there's basically _only data_, becomes way simpler if you don't try do force it into something OO-like but just stay with (usefully typed) _data_ in Haskell.

So as this example obviously doesn't bring us any further let's consider something where OOP _does_ make sense. Widget toolkits come to the mind. Something like

    class Widget;
    
    class Container : public Widget {
      std::vector<std::unique_ptr<Widget>> children;
     public:
      // getters ...
    };
    class Paned : public Container { public:
      Rectangle childBoundaries(int) const;
    };
    class ReEquipable : public Container { public:
      void pushNewChild(std::unique_ptr<Widget>&&);
      void popChild(int);
    };
    class HJuxtaposition: public Paned, public ReEquipable { ... };
    

Why OO makes sense here? First, it readily allows us to store a heterogeneous collection of widgets. That's actually not easy to achieve in Haskell, but before trying it, you might ask yourself if you really need it. For certain containers, it's perhaps not so desirable to allow this, after all. In Haskell, _parametric polymorphism_ is very nice to use. For any given type of widget, we observe the functionality of `Container` pretty much reduces to a simple list. So why not just use a list, wherever you require a `Container`?

Of course, in this example, you'll probably find you _do_ need heterogeneous containers; the most direct way to obtain them is `{-# LANGUAGE ExistentialQuantification #-}`:

    data GenericWidget = GenericWidget { forall w . Widget w => getGenericWidget :: w }
    

In this case `Widget` would be a type class (might be a rather literal translation of the abstract class `Widget`). In Haskell this is rather a last-resort thing to do, but might be right here.

`Paned` is more of an interface. We might use another type class here, basically transliterating the C++ one:

    class Paned c where
      childBoundaries :: c -> Int -> Maybe Rectangle
    

`ReEquipable` is more difficult, because its methods actually mutate the container. That is _obviously_ problematic in Haskell. But again you might find it's not necessary: if you've substituted the `Container` class by plain lists, you might be able to do the updates as pure-functional updates.

Probably though, this would be too inefficient for the task at hand. Fully discussing ways to do mutable updates efficiently would be too much for the scope of this answer, but such ways exists, e.g. using [`lenses`](http://hackage.haskell.org/package/lens).

Summary
-------

OO doesn't translate too well to Haskell. There isn't one simple generic isomorphism, only multiple approximations amongst which to choose requires experience. As often as possible, you should avoid approaching the problem from an OO angle alltogether and think about data, functions, monad layers instead. It turns out this gets you very far in Haskell. Only in a few applications, OO is so natural that it's worth pressing it into the language.

* * *

†Sorry, this subject always drives me into strong-opinion rant mode...

‡These paranoia are partly motivated by the troubles of mutability, which don't arise in Haskell.
