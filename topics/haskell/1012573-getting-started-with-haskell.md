
# Getting started with Haskell

## Question
      
For a few days I've tried to wrap my head around the functional programming paradigm in Haskell. I've done this by reading tutorials and watching screencasts, but nothing really seems to stick. Now, in learning various imperative/OO languages (like C, Java, PHP), exercises have been a good way for me to go. But since I don't really know what Haskell is capable of and because there are many new concepts to utilize, I haven't known where to start.

So, how did you learn Haskell? What made you really "break the ice"? Also, any good ideas for beginning exercises?
## Answer
      
I'm going to order this guide by the level of skill you have in haskell, going from an absolute beginner right up to an expert. Note that this process will take many months (years?), so it is rather long.

**Absolute Beginner**

Firstly, haskell is capable of anything, with enough skill. It is very fast (behind only c and c++ in my experience), and can be used for anything from simulations to servers, guis and web applications.

However there are some problems that are easier to write for a beginner in haskell than others. Mathematical problems and list process programs are good candidates for this, as they only require the most basic of haskell knowledge to be able to write.

Firstly, some good guides to learning the very basics of haskell are the [happy learn haskell tutorial](http://www.happylearnhaskelltutorial.com) and the first 6 chapters of [learn you a haskell](http://learnyouahaskell.com/chapters). While reading these, it is a very good idea to also be solving simple problems with what you know.

Another two good resources are [Haskell Programming from first principles](http://haskellbook.com/), and [Programming in Haskell](http://www.cs.nott.ac.uk/~pszgmh/pih.html). They both come with exercises for each chapter, so you have small simple problems matching what you learned on the last few pages.

A good list of problems to try is the [haskell 99 problems page](http://haskell.org/haskellwiki/H-99:_Ninety-Nine_Haskell_Problems). These start off very basic, and get more difficult as you go on. It is very good practice doing a lot of those, as they let you practice your skills in recursion and higher order functions. I would recommend skipping any problems that require randomness as that is a bit more difficult in haskell. Check [this SO question](https://stackoverflow.com/questions/5683911/simple-haskell-unit-testing) in case you want to test your solutions with QuickCheck (see _Intermediate_ below).

Once you have done a few of those, you could move on to doing a few of the [Project Euler](http://projecteuler.net/index.php?section=problems) problems. These are sorted by how many people have completed them, which is a fairly good indication of difficulty. These test your logic and haskell more than the previous problems, but you should still be able to do the first few. A big advantage haskell has with these problems is Integers aren't limited in size. To complete some of these problems, it will be useful to have read chapters 7 and 8 of learn you a haskell as well.

**Beginner**

After that you should have a fairly good handle on recursion and higher order functions, so it would be a good time to start doing some more real world problems. A very good place to start is [Real World Haskell](http://book.realworldhaskell.org/) (online book, you can also purchase a hard copy). I found the first few chapters introduced too much too quickly for someone who has never done functional programming/used recursion before. However with the practice you would have had from doing the previous problems you should find it perfectly understandable.

Working through the problems in the book is a great way of learning how to manage abstractions and building reusable components in haskell. This is vital for people used to object-orientated (oo) programming, as the normal oo abstraction methods (oo classes) don't appear in haskell (haskell has type classes, but they are very different to oo classes, more like oo interfaces). I don't think it is a good idea to skip chapters, as each introduces a lot new ideas that are used in later chapters.

After a while you will get to chapter 14, the dreaded monads chapter (dum dum dummmm). Almost everyone who learns haskell has trouble understanding monads, due to how abstract the concept is. I can't think of any concept in another language that is as abstract as monads are in functional programming. Monads allows many ideas (such as IO operations, computations that might fail, parsing,...) to be unified under one idea. So don't feel discouraged if after reading the monads chapter you don't really understand them. I found it useful to read many different explanations of monads; each one gives a new perspective on the problem. Here is a very good [list of monad tutorials](https://wiki.haskell.org/Tutorials#Using_monads). I highly recommend the [All About Monads](https://wiki.haskell.org/All_About_Monads), but the others are also good.

Also, it takes a while for the concepts to truly sink in. This comes through use, but also through time. I find that sometimes sleeping on a problem helps more than anything else! Eventually, the idea will click, and you will wonder why you struggled to understand a concept that in reality is incredibly simple. It is awesome when this happens, and when it does, you might find haskell to be your favorite imperative programming language :)

To make sure that you are understanding Haskell type system perfectly, you should try to solve [20 intermediate haskell exercises](http://blog.tmorris.net/posts/20-intermediate-haskell-exercises/). Those exercises using fun names of functions like "furry" and "banana" and helps you to have a good understanding of some basic functional programming concepts if you don't have them already. Nice way to spend your evening with list of paper covered with arrows, unicorns, sausages and furry bananas.

**Intermediate**

Once you understand Monads, I think you have made the transition from a beginner haskell programmer to an intermediate haskeller. So where to go from here? The first thing I would recommend (if you haven't already learnt them from learning monads) is the various types of monads, such as Reader, Writer and State. Again, Real world haskell and All about monads gives great coverage of this. To complete your monad training learning about monad transformers is a must. These let you combine different types of Monads (such as a Reader and State monad) into one. This may seem useless to begin with, but after using them for a while you will wonder how you lived without them.

Now you can finish the real world haskell book if you want. Skipping chapters now though doesn't really matter, as long as you have monads down pat. Just choose what you are interested in.

With the knowledge you would have now, you should be able to use most of the packages on cabal (well the documented ones at least...), as well as most of the libraries that come with haskell. A list of interesting libraries to try would be:

*   [Parsec](https://wiki.haskell.org/Parsec): for parsing programs and text. Much better than using regexps. Excellent documentation, also has a real world haskell chapter.
    
*   [Quickcheck](http://www.cse.chalmers.se/~rjmh/QuickCheck/): A very cool testing program. What you do is write a predicate that should always be true (eg `length (reverse lst) == length lst`). You then pass the predicate the quickCheck, and it will generate a lot of random values (in this case lists) and test that the predicate is true for all results. See also the [online manual](http://www.cse.chalmers.se/~rjmh/QuickCheck/manual.html).
    
*   [HUnit](http://hunit.sourceforge.net/): Unit testing in haskell.
    
*   [gtk2hs](http://projects.haskell.org/gtk2hs/): The most popular gui framework for haskell, lets you write gtk applications in haskell.
    
*   [happstack](http://happstack.com/): A web development framework for haskell. Doesn't use databases, instead a data type store. Pretty good docs (other popular frameworks would be [snap](http://snapframework.com/) and [yesod](http://www.yesodweb.com/)).
    

Also, there are many concepts (like the Monad concept) that you should eventually learn. This will be easier than learning Monads the first time, as your brain will be used to dealing with the level of abstraction involved. A very good overview for learning about these high level concepts and how they fit together is the [Typeclassopedia](http://www.haskell.org/haskellwiki/Typeclassopedia).

*   Applicative: An interface like Monads, but less powerful. Every Monad is Applicative, but not vice versa. This is useful as there are some types that are Applicative but are not Monads. Also, code written using the Applicative functions is often more composable than writing the equivalent code using the Monad functions. See [Functors, Applicative Functors and Monoids](http://learnyouahaskell.com/functors-applicative-functors-and-monoids#functors-redux) from the learn you a haskell guide.
    
*   [Foldable](http://www.haskell.org/ghc/docs/latest/html/libraries/base/Data-Foldable.html),[Traversable](http://www.haskell.org/ghc/docs/latest/html/libraries/base/Data-Traversable.html): Typeclasses that abstract many of the operations of lists, so that the same functions can be applied to other container types. See also the [haskell wiki explaination](https://wiki.haskell.org/Foldable_and_Traversable).
    
*   [Monoid](https://wiki.haskell.org/Monoid): A Monoid is a type that has a zero (or mempty) value, and an operation, notated `<>` that joins two Monoids together, such that `x <> mempty = mempty <> x = x` and `x <> (y <> z) = (x <> y) <> z`. These are called identity and associativity laws. Many types are Monoids, such as numbers, with `mempty = 0` and `<> = +`. This is useful in many situations.
    
*   [Arrows](http://www.haskell.org/arrows/): Arrows are a way of representing computations that take an input and return an output. A function is the most basic type of arrow, but there are many other types. The library also has many very useful functions for manipulating arrows - they are very useful even if only used with plain old haskell functions.
    
*   [Arrays](https://wiki.haskell.org/Modern_array_libraries): the various mutable/immutable arrays in haskell.
    
*   [ST Monad](https://wiki.haskell.org/Monad/ST): lets you write code with a mutable state that runs very quickly, while still remaining pure outside the monad. See the link for more details.
    
*   FRP: Functional Reactive Programming, a new, experimental way of writing code that handles events, triggers, inputs and outputs (such as a gui). I don't know much about this though. [Paul Hudak's talk about yampa](http://vimeo.com/96744621) is a good start.
    

There are a lot of new language features you should have a look at. I'll just list them, you can find lots of info about them from google, the [haskell wikibook](http://en.wikibooks.org/wiki/Haskell), the haskellwiki.org site and [ghc documentation](https://wiki.haskell.org/GHC).

*   Multiparameter type classes/functional dependencies
*   Type families
*   Existentially quantified types
*   Phantom types
*   GADTS
*   others...

A lot of haskell is based around [category theory](http://en.wikipedia.org/wiki/Category_theory), so you may want to look into that. A good starting point is [Category Theory for Computer Scientist](https://rads.stackoverflow.com/amzn/click/0262660717). If you don't want to buy the book, the author's related [article](http://repository.cmu.edu/cgi/viewcontent.cgi?article=2846&context=compsci) is also excellent.

Finally you will want to learn more about the various haskell tools. These include:

*   [ghc](https://wiki.haskell.org/GHC) (and all its features)
*   [cabal](http://www.haskell.org/cabal/): the haskell package system
*   [darcs](http://darcs.net/): a distributed version control system written in haskell, very popular for haskell programs.
*   [haddock](http://www.haskell.org/haddock/): a haskell automatic documentation generator

While learning all these new libraries and concepts, it is very useful to be writing a moderate-sized project in haskell. It can be anything (eg a small game, data analyser, website, [compiler](http://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours)). Working on this will allow you to apply many of the things you are now learning. You stay at this level for ages (this is where I'm at).

**Expert**

It will take you years to get to this stage (hello from 2009!), but from here I'm guessing you start writing phd papers, new ghc extensions, and coming up with new abstractions.

**Getting Help**

Finally, while at any stage of learning, there are multiple places for getting information. These are:

*   the #haskell irc channel
*   the [mailing lists](https://wiki.haskell.org/Mailing_lists). These are worth signing up for just to read the discussions that take place - some are very interesting.
*   other places listed on the haskell.org home page

**Conclusion**

Well this turned out longer than I expected... Anyway, I think it is a very good idea to become proficient in haskell. It takes a long time, but that is mainly because you are learning a completely new way of thinking by doing so. It is not like learning ruby after learning java, but like learning java after learning c. Also, I am finding that my object-orientated programming skills have improved as a result of learning haskell, as I am seeing many new ways of abstracting ideas.
    