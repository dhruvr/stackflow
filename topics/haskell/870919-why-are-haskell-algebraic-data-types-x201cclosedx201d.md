
# Why are Haskell algebraic data types &#x201C;closed&#x201D;?

## Question
        
Correct me if I'm wrong, but it seems like algebraic data types in Haskell are useful in many of the cases where you would use classes and inheritance in OO languages. But there is a big difference: once an algebraic data type is declared, it can not be extended elsewhere. It is "closed". In OO, you can extend already defined classes. For example:

    data Maybe a = Nothing | Just a
    

There is no way that I can somehow add another option to this type later on without modifying this declaration. So what are the benefits of this system? It seems like the OO way would be much more extensible.

## Answer
        
The fact that ADT are closed makes it a lot easier to write total functions. That are functions that always produce a result, for all possible values of its type, eg.

    maybeToList :: Maybe a -> [a]
    maybeToList Nothing  = []
    maybeToList (Just x) = [x]
    

If `Maybe` were open, someone could add a extra constructor and the `maybeToList` function would suddenly break.

In OO this isn't an issue, when you're using inheritance to extend a type, because when you call a function for which there is no specific overload, it can just use the implementation for a superclass. I.e., you can call `printPerson(Person p)` just fine with a `Student` object if `Student` is a subclass of `Person`.

In Haskell, you would usually use encapsulation and type classes when you need to extent your types. For example:

    class Eq a where
       (==) :: a -> a -> Bool
    
    instance Eq Bool where
      False == False = True
      False == True  = False
      True  == False = False
      True  == True  = True
    
    instance Eq a => Eq [a] where
      []     == []     = True
      (x:xs) == (y:ys) = x == y && xs == ys
      _      == _      = False
    

Now, the `==` function is completely open, you can add your own types by making it an instance of the `Eq` class.

* * *

Note that there has been work on the idea of [extensible datatypes](http://www.haskell.org/haskellwiki/Extensible_datatypes), but that is definitely not part of Haskell yet.
