
# How to catch a no parse exception from the read function in Haskell?

## Question
        
In my Haskell program, I want to read in a value given by the user using the `getLine` function. I then want to use the `read` function to convert this value from a string to the appropriate Haskell type. How can I catch parse errors thrown by the `read` function and ask the user to reenter the value?

Am I right in thinking that this is not an "IO Error" because it is not an error caused by the IO system not functioning correctly? It is a semantic error, so I can't use IO error handling mechanisms?

## Answer
        
You don't want to. You want to use [reads](http://hackage.haskell.org/packages/archive/base/4.0.0.0/doc/html/Text-Read.html#v%3Areads) instead, possibly like that:

    maybeRead = fmap fst . listToMaybe . reads
    

(though you might want to error out if the second element of the tuple is not `""`, that is, if there's a remaining string, too)

The _reason_ why you want to use reads instead of catching `error` exceptions is that exceptions in pure code are evil, because it's _very_ easy to attempt to catch them in the wrong place: Note that they only fly when they are forced, not before. Locating where that is can be a non-trivial exercise. That's (one of the reasons) why Haskell programmers like to keep their code total, that is, terminating and exception-free.

You might want to have a look at a proper parsing framework (e.g. [parsec](http://www.haskell.org/haskellwiki/Parsec)) and [haskeline](http://hackage.haskell.org/package/haskeline), too.
