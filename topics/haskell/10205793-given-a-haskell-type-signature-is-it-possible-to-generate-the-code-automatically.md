
# Given a Haskell type signature, is it possible to generate the code automatically?

## Question
        
What it says in the title. If I write a type signature, is it possible to algorithmically generate an expression which has that type signature?

It seems plausible that it might be possible to do this. We already know that if the type is a special-case of a library function's type signature, Hoogle can find that function algorithmically. On the other hand, many simple problems relating to general expressions are actually unsolvable (e.g., it is impossible to know if two functions do the same thing), so it's hardly implausible that this is one of them.

It's probably bad form to ask several questions all at once, but I'd like to know:

*   Can it be done?
    
*   If so, how?
    
*   If not, are there any restricted situations where it becomes possible?
    
*   It's quite possible for two distinct expressions to have the same type signature. Can you compute _all_ of them? Or even _some_ of them?
    
*   Does anybody have working code which does this stuff for real?

## Answer
        
[Djinn](http://lambda-the-ultimate.org/node/1178) does this for a restricted subset of Haskell types, corresponding to a first-order logic. It can't manage recursive types or types that require recursion to implement, though; so, for instance, it can't write a term of type `(a -> a) -> a` (the type of `fix`), which corresponds to the proposition "if _a_ implies _a_, then _a_", which is clearly false; you can use it to prove anything. Indeed, this is why `fix` gives rise to ⊥.

If you _do_ allow `fix`, then writing a program to give a term of any type is trivial; the program would simply print `fix id` for every type.

Djinn is mostly a toy, but it can do some fun things, like deriving the correct `Monad` instances for `Reader` and `Cont` given the types of `return` and `(>>=)`. You can try it out by installing the [djinn](http://hackage.haskell.org/package/djinn) package, or using [lambdabot](http://www.haskell.org/haskellwiki/Lambdabot), which integrates it as the `@djinn` command.
