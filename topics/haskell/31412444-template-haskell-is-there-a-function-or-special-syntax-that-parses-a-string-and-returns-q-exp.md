
# Template Haskell: Is there a function (or special syntax) that parses a String and returns Q Exp?

## Question
        
I am trying to learn a bit of Template Haskell and Quasi Quotation, and I am looking for a function that takes a `String` and parses it to `Q Exp`, so the type is:

    String -> Q Exp
    

Tried searching hoogle, but the results I saw had to do with lifting String literals to `Q Exp`, and the closest I found was `Language.Haskell.TH.dyn` which does quite what I want, but only for a single variable.

Are there other options? E.g. a special syntax? I'm just in the process of familiarizing myself with `[||]` and `$()`, so maybe there is something for this purpose too?

An example of how I imagine it would work:

    runQ (parse "(1+)") == InfixE (Just (LitE (IntegerL 1))) (VarE GHC.Num.+) Nothing
    

Also, I am aware of this

    runQ [| (1+) |] == InfixE (Just (LitE (IntegerL 1))) (VarE GHC.Num.+) Nothing
    

but this wont work with variable strings because -- understandably -- the string inside is taken as a literal.

    runQ [| "(1+)" |] == LitE (StringL "(1+)")
    

**Edit (2015-07-25):** I've started using `haskell-src-meta`, and it seems to work well so far. However it does take quite a bit of time to `cabal install` (about 10 minutes on my machine). Which is a shame, my package is actually rather small, and I would like if install could be quick. Anyone knows of a solution that has smaller dependencies?

## Answer
        
As everyone has already said [`haskell-src-meta`](https://hackage.haskell.org/package/haskell-src-meta/docs/Language-Haskell-Meta-Parse.html) provides

    parsePat :: String -> Either String Pat
    parseExp :: String -> Either String Exp
    parseType :: String -> Either String Type
    parseDecs :: String -> Either String [Dec]
    

where `Pat`, `Exp`, `Type`, and `Dec` are the same as from [`Language.Haskell.TH.Syntax`](https://hackage.haskell.org/package/template-haskell/docs/Language-Haskell-TH-Syntax.html).

* * *

> Why doesn't GHC expose its own parser?

It does. Fire up GHCi with `ghci -package ghc` (`ghc` is a hidden package by default) and you can import the [`Parser`](https://downloads.haskell.org/~ghc/8.0.1/docs/html/libraries/ghc-8.0.1/Parser.html). It has functions to parse `String` into preliminary ASTs (whose data declarations are in [`HsSyn`](https://downloads.haskell.org/~ghc/8.0.1/docs/html/libraries/ghc-8.0.1/HsSyn.html)) for patterns, expressions, types, and declarations.

> OK, then why does there not exist a library that uses this parser and converts its output to be the AST from `template-haskell` (the one in [`Language.Haskell.TH.Syntax`](https://hackage.haskell.org/package/template-haskell/docs/Language-Haskell-TH-Syntax.html))?

Looking inside [`HsSyn`](https://downloads.haskell.org/~ghc/8.0.1/docs/html/libraries/ghc-8.0.1/HsSyn.html), its obvious that the AST isn't quite the same as the one in [`Language.Haskell.TH.Syntax`](https://hackage.haskell.org/package/template-haskell/docs/Language-Haskell-TH-Syntax.html). Open up both [`HsExpr`](https://downloads.haskell.org/~ghc/8.0.1/docs/html/libraries/ghc-8.0.1/HsExpr.html#t:HsExpr) and [`Exp`](https://hackage.haskell.org/package/template-haskell/docs/Language-Haskell-TH-Syntax.html#t:Exp) and side by side you'll see that the latter is filled with types like `PostTc id <some-other-type>` and `PostRn id <some-other-type>`. As the AST is passed from the parser to the renamer to the type checker, these bits and pieces are all slowly filled in. For example, we don't even know the fixities of operators until we get to type-checking!

In order to make the functions we want, we would need to run much more than just the parser (at least the renamer and type checker too, maybe more). Imagine that: every time you want to parse even a small expression like `"1 + 2"` you'll still have to type check a bunch of imports. Even then, converting back to the [`Language.Haskell.TH.Syntax`](https://hackage.haskell.org/package/template-haskell/docs/Language-Haskell-TH-Syntax.html) wouldn't be a walk in the park: GHC has a variety of peculiarities like its own special global way of storing names and identifiers.

> Hmmm... but what does GHC do with quasi-quotes?

That's the cool part! Unlike `Exp`, `HsExpr` has [`HsSplice`](https://downloads.haskell.org/~ghc/8.0.1/docs/html/libraries/ghc-8.0.1/HsExpr.html#t:HsSplice) for representing splices. Look at the types for the first two constructors:

    HsTypedSplice :: id -> LHsExpr id -> HsSplice id.   -- things like [|| 1 + 2 ||]
    HsUntypedSplice :: id -> LHsExpr id -> HsSplice id  -- things like [| 1 + 2 |]
    

Notice that they aren't storing `String`, they are storing an AST already! Splices get parsed at the same time as the rest of the AST. And just like the rest of the AST, the splices will get passed along to the renamer, type checker, etc. where missing information will be filled in.

> So is it fundamentally impossible to use GHC's parser

Probably not. But extricating it from the rest of GHC may be quite difficult. If to use GHC's parser we have to also run the type-checker and the renamer, it may be more elegant and simple to just use a standalone parser like `haskell-src-exts` (which is what `Haskell-src-meta` depends on) that is able to do everything in one pass (fixities, for example, are one of the things you have to give _ahead of time_ to this parser).
