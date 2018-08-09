
# What&apos;s so bad about Template Haskell?

## Question
        
It seems that Template Haskell is often viewed by the Haskell community as an unfortunate convenience. It's hard to put into words exactly what I have observed in this regard, but consider these few examples

*   Template Haskell listed under "The Ugly (but necessary)" in response to the question [Which Haskell (GHC) extensions should users use/avoid?](https://stackoverflow.com/a/10849782/208257)
*   Template Haskell considered a temporary/inferior solution in [Unboxed Vectors of newtype'd values](http://www.haskell.org/pipermail/libraries/2012-May/017815.html) thread (libraries mailing list)
*   Yesod is often criticized for relying too much on Template Haskell (see [the blog post](http://www.yesodweb.com/blog/2011/04/yesod-template-haskell) in response to this sentiment)

I've seen various blog posts where people do pretty neat stuff with Template Haskell, enabling prettier syntax that simply wouldn't be possible in regular Haskell, as well as tremendous boilerplate reduction. So why is it that Template Haskell is looked down upon in this way? What makes it undesirable? Under what circumstances should Template Haskell be avoided, and why?

## Answer
        
One reason for avoiding Template Haskell is that it as a whole isn't type-safe, at all, thus going against much of "the spirit of Haskell." Here are some examples of this:

*   You have no control over what kind of Haskell AST a piece of TH code will generate, beyond where it will appear; you can have a value of type [`Exp`](http://hackage.haskell.org/packages/archive/template-haskell/latest/doc/html/Language-Haskell-TH-Syntax.html#t:Exp), but you don't know if it is an expression that represents a `[Char]` or a `(a -> (forall b . b -> c))` or whatever. TH would be more reliable if one could express that a function may only generate expressions of a certain type, or only function declarations, or only data-constructor-matching patterns, etc.
*   You can generate expressions that don't compile. You generated an expression that references a free variable `foo` that doesn't exist? Tough luck, you'll only see that when actually using your code generator, and only under the circumstances that trigger the generation of that particular code. It is very difficult to unit test, too.

TH is also outright dangerous:

*   Code that runs at compile-time can do arbitrary `IO`, including launching missiles or stealing your credit card. You don't want to have to look through every cabal package you ever download in search for TH exploits.
*   TH can access "module-private" functions and definitions, completely breaking encapsulation in some cases.

Then there are some problems that make TH functions less fun to use as a library developer:

*   TH code isn't always composable. Let's say someone makes a generator for lenses, and more often than not, that generator will be structured in such a way that it can only be called directly by the "end-user," and not by other TH code, by for example taking a list of type constructors to generate lenses for as the parameter. It is tricky to generate that list in code, while the user only has to write `generateLenses [''Foo, ''Bar]`.
*   Developers don't even **know** that TH code can be composed. Did you know that you can write `forM_ [''Foo, ''Bar] generateLens`? `Q` is just a monad, so you can use all of the usual functions on it. Some people don't know this, and because of that, they create multiple overloaded versions of essentially the same functions with the same functionality, and these functions lead to a certain bloat effect. Also, most people write their generators in the `Q` monad even when they don't have to, which is like writing `bla :: IO Int; bla = return 3`; you are giving a function more "environment" than it needs, and clients of the function are required to provide that environment as an effect of that.

Finally, there are some things that make TH functions less fun to use as an end-user:

*   Opacity. When a TH function has type `Q Dec`, it can generate absolutely anything at the top-level of a module, and you have absolutely no control over what will be generated.
*   Monolithism. You can't control how much a TH function generates unless the developer allows it; if you find a function that generates a database interface **and** a JSON serialization interface, you can't say "No, I only want the database interface, thanks; I'll roll my own JSON interface"
*   Run time. TH code takes a relatively long time to run. The code is interpreted anew every time a file is compiled, and often, a ton of packages are required by the running TH code, that have to be loaded. This slows down compile time considerably.
