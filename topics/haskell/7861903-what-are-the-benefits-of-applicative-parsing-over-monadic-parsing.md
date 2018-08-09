
# What are the benefits of applicative parsing over monadic parsing?

## Question
        
There seems to be a consensus that you should use Parsec as an applicative rather than a monad. What are the benefits of applicative parsing over monadic parsing?

*   style
*   performance
*   abstraction

Is monadic parsing out?

## Answer
        
The main difference between monadic and applicative parsing is in how sequential composition is handled. In the case of an applicative parser, we use `(<*>)`, whereas with a monad we use `(>>=)`.

    (<*>) :: Parser (a -> b) -> Parser a -> Parser b
    (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    

The monadic approach is more flexible, because it allows the grammar of the second part to depend on the result from the first one, but we rarely need this extra flexibility in practice.

You might think that having some extra flexibility can't hurt, but in reality it can. It prevents us from doing useful static analysis on a parser without running it. For example, let's say we want to know whether a parser can match the empty string or not, and what the possible first characters can be in a match. We want functions

    empty :: Parser a -> Bool
    first :: Parser a -> Set Char
    

With an applicative parser, we can easily answer this question. (I'm cheating a little here. Imagine we have a data constructors corresponding to `(<*>)` and `(>>=)` in our candidate parser "languages").

    empty (f <*> x) = empty f && empty x
    first (f <*> x) | empty f   = first f `union` first x
                    | otherwise = first f
    

However, with a monadic parser we don't know what the grammar of the second part is without knowing the input.

    empty (x >>= f) = empty x && empty (f ???)
    first (x >>= f) | empty x   = first x `union` first (f ???)
                    | otherwise = first x
    

By allowing more, we're able to reason less. This is similar to the choice between dynamic and static type systems.

But what is the point of this? What might we use this extra static knowledge for? Well, we can for example use it to avoid backtracking in LL(1) parsing by comparing the next character to the `first` set of each alternative. We can also determine statically whether this would be ambiguous by checking if the `first` sets of two alternatives overlap.

Another example is that it can be used for error recovery, as shown in the paper [Deterministic, Error-Correcting Combinator Parsers](http://www.staff.science.uu.nl/~swier101/Papers/1996/LL1.pdf) by S. Doaitse Swierstra and Luc Duponcheel.

Usually, however, the choice between applicative and monadic parsing has already been made by the authors of the parsing library you're using. When a library such as Parsec exposes both interfaces, the choice of which one to use is purely a stylistic one. In some cases applicative code is easier to read than monadic code and sometimes it's the other way round.
