
# What types of problems helps &#x201C;higher-kinded polymorphism&#x201D; solve better?

## Question
        
As I read through some sections in [History of Haskell](https://www.microsoft.com/en-us/research/wp-content/uploads/2016/07/history.pdf), I came across:

> However, higher-kinded polymorphism has independent utility: it is entirely possible, and occasionally very useful, to declare data types parameterised over higher kinds, such as:

    data ListFunctor f a = Nil | Cons a (f a)
    

Knowing "basic" ADTs I was a bit puzzled here, my "guess" was that the part in parens suggests a "parametric"/"dynamic" _unary data constructor_ `f`? So any data constructor of kind `* -> *` that "can accept" type `a`? Is my thinking correct or am I misinterpreting the syntax? I know I'm "just guessing" but I'm hopeful to gain a "lay-programmer" intuition on this capability here, some sample scenario needing (or benefiting immensively from) this ;) mostly I can imagine (just not in what exact manner) this allowing more flexibility in those "small embedded versatile recursable config language"-ADTs that Haskell makes such a pleasure to formulate and write `evals` for.. close?

In GHCi, `:i ListFunctor` on the above gives:

    type role ListFunctor representational nominal
    data ListFunctor (f :: * -> *) a = Nil | Cons a (f a)
    

So this seems to be what's "inferred" from the crisper `data` declaration.

## Answer
        
Yes, `f` can be any unary type constructor.

For instance `ListFunctor [] Int` or `ListFunctor Maybe Char` are well-kinded.

`f` can also be any n-ary type constructor with (n-1) arguments partially applied.

For instance `ListFunctor ((->) Bool) Int` or `ListFunctor (Either ()) Char` are well-kinded.

The basic kinding system is quite simple. If `F :: * -> * -> ... -> *`, then `F` expects type arguments. If `G :: (* -> *) -> *`, then `G` expects any thing of kind `* -> *` including unary type constructor and partial applications as the ones shown above. And so on.

* * *

A problem which is nicely solved by higher kinds is configuration options. Assume we have a record

    data Opt = Opt 
       { opt1 :: Bool
       , opt2 :: String
       -- many other fields here
       }
    

Now, configuration settings can be found in a file and/or passed through the command line and/or in environment variables. During the parsing of all these settings sources, we need to cope with the fact that not all sources define all options. Hence, we need a more lax type to represents subsets of configuration settings:

    data TempOpt = TempOpt 
       { tempOpt1 :: Maybe Bool
       , tempOpt2 :: Maybe String
       -- many other fields here
       }
    
    -- merge all options in one single configuration, or fail
    finalize :: [TempOpt] -> Maybe Opt
    ...
    

This is horrible, since it duplicates all the options! We would be tempted to remove the `Opt` type, and only use the weaker `TempOpt`, to reduce clutter. However, by doing this we will need to use some partial accessor like `fromJust` every time we need to access the value of an option in our program, even after the initial configuration handling part.

We can instead resort to higher kinds:

    data FOpt f = FOpt 
       { opt1 :: f Bool
       , opt2 :: f String
       -- many other fields here
       }
    type Opt = FOpt Identity
    type TempOpt = FOpt Maybe
    
    -- as before: merge all options in one single configuration, or fail
    finalize :: [TempOpt] -> Maybe Opt
    ...
    

No more duplication. After we `finalize` the configuration settings, we get the static guarantee that settings are always present. We can now use the _total_ accessor `runIdentity` to get them, instead of the dangerous `fromJust`.
