
# Is monad bind (&gt;&gt;=) operator closer to function composition (chaining) or function application?

## Question
        
In many articles I have read that monad `>>=` operator is a way to represent function composition. But for me it is closer to some kind of advanced function application

    ($)   :: (a -> b) -> a -> b
    (>>=) :: Monad m => m a -> (a -> m b) -> m b
    

For composition we have

    (.)   :: (b -> c) -> (a -> b) -> a -> c
    (>=>) :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c
    

Please clarify.

## Answer
        
Clearly, `>>=` is not a way to represent _function composition_. Function composition is simply done with `.`. However, I don't think any of the articles you've read meant this, either.

What they meant was “upgrading” function composition to work directly with “monadic functions”, i.e. functions of the form `a -> m b`. The technical term for such functions is _Kleisli arrows_, and indeed they can be composed with `<=<` or `>=>`. (Alternatively, you can use the [`Category` instance](http://hackage.haskell.org/package/base-4.8.1.0/docs/Control-Arrow.html#t:Kleisli), then you can also compose them with `.` or `>>>`.)

However, talking about arrows / categories tends to be confusing especially to beginners, just like [point-free definitions](https://wiki.haskell.org/Point-free) of ordinary functions are often confusing. Luckily, Haskell allows us to express functions also in a more familiar style that focuses on the _results_ of functions, rather the functions themselves as abstract morphisms†. It's done with lambda abstraction: instead of

    q = h . g . f
    

you may write

    q = (\x -> (\y -> (\z -> h z) (g y)) (f x))
    

...of course the preferred style would be (this being only syntactic sugar for lambda abstraction!)‡

    q x = let y = f x
              z = g y
          in h z
    

Note how, in the lambda expression, basically composition was replaced by application:

    q = \x -> (\y -> (\z -> h z) $ g y) $ f x
    

Adapted to Kleisli arrows, this means instead of

    q = h <=< g <=< f
    

you write

    q = \x -> (\y -> (\z -> h z) =<< g y) =<< f x
    

which again looks of course much nicer with flipped operators or syntactic sugar:

    q x = do y <- f x
             z <- g y
             h z
    

So, indeed, `=<<` is to `<=<` like `$` is to `.`. The reason it still makes sense to call it a composition operator is that, apart from “applying to values”, the `>>=` operator also does the nontrivial bit about Kleisli arrow composition, which function composition doesn't need: joining the monadic layers.

* * *

†The reason this works is that **Hask** is a [cartesian closed category](https://en.wikipedia.org/wiki/Cartesian_closed_category), in particular a [well-pointed category](https://en.wikipedia.org/wiki/Well-pointed_category). In such a category, arrows can, broadly speaking, be defined by the collection of all their results when applied to simple argument values.

‡@adamse remarks that `let` is not really syntactic sugar for lambda abstraction. This is particularly relevant in case of recursive definitions, which you can't directly write with a lambda. But in simple cases like this here, `let` does behave like syntactic sugar for lambdas, just like `do` notation is syntactic sugar for lambdas and `>>=`. (BTW, there's an extension which allows recursion [even in `do` notation](https://wiki.haskell.org/MonadFix)... it circumvents the lambda-restriction by using fixed-point combinators.)
