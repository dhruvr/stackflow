
# Non-Trivial Lazy Evaluation

## Question
        
I'm currently digesting the nice presentation _Why learn Haskell?_ by Keegan McAllister. There he uses the snippet

    minimum = head . sort
    

as an illustration of Haskell's lazy evaluation by stating that `minimum` has _time-complexity_ O(n) in Haskell. However, I think the example is kind of academic in nature. I'm therefore asking for a more practical example where it's not trivially apparent that most of the _intermediate_ calculations are thrown away.

## Answer
        
*   Have you ever written an AI? Isn't it annoying that you have to thread pruning information (e.g. maximum depth, the minimum cost of an adjacent branch, or other such information) through the tree traversal function? This means you have to write a new tree traversal every time you want to improve your AI. That's dumb. With lazy evaluation, this is no longer a problem: write your tree traversal function once, to produce a huge (maybe even infinite!) game tree, and let your consumer decide how much of it to consume.
    
*   Writing a GUI that shows lots of information? Want it to run fast anyway? In other languages, you might have to write code that renders only the visible scenes. In Haskell, you can write code that renders the whole scene, and then later choose which pixels to observe. Similarly, rendering a complicated scene? Why not compute an infinite sequence of scenes at various detail levels, and pick the most appropriate one as the program runs?
    
*   You write an expensive function, and decide to memoize it for speed. In other languages, this requires building a data structure that tracks which inputs for the function you know the answer to, and updating the structure as you see new inputs. Remember to make it thread safe -- if we really need speed, we need parallelism, too! In Haskell, you build an infinite data structure, with an entry for each possible input, and evaluate the parts of the data structure that correspond to the inputs you care about. Thread safety comes for free with purity.
    
*   Here's one that's perhaps a bit more prosaic than the previous ones. Have you ever found a time when `&&` and `||` weren't the only things you wanted to be short-circuiting? I sure have! For example, I love the `<|>` function for combining `Maybe` values: it takes the first one of its arguments that actually has a value. So `Just 3 <|> Nothing = Just 3`; `Nothing <|> Just 7 = Just 7`; and `Nothing <|> Nothing = Nothing`. Moreover, it's short-circuiting: if it turns out that its first argument is a `Just`, it won't bother doing the computation required to figure out what its second argument is.
    
    And `<|>` isn't built in to the language; it's tacked on by a library. That is: laziness allows you to write _brand new_ short-circuiting forms. (Indeed, in Haskell, even the short-circuiting behavior of `(&&)` and `(||)` aren't built-in compiler magic: they arise naturally from the semantics of the language plus their definitions in the standard libraries.)
    

In general, the common theme here is that you can separate the production of values from the determination of which values are _interesting to look at_. This makes things more composable, because the choice of what is interesting to look at need not be known by the producer.
