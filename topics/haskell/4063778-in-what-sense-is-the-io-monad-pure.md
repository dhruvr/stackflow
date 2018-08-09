
# In what sense is the IO Monad pure?

## Question
        
I've had the IO monad described to me as a State monad where the state is "the real world". The proponents of this approach to IO argue that this makes IO operations pure, as in referentially transparent. Why is that? From my perspective it appears that code inside the IO monad have plenty of observable side effects. Also, isn't it possible to describe pretty much any non-pure function like a function of the real world? For example, can't we think of, say, C's malloc as being a function that takes a _RealWorld_ and an Int and returns a pointer and a _RealWorld_, only just like in the IO monad the _RealWorld_ is implicit?

Note: I know what a monad is and how it's used. Please don't respond with a link to a random monad tutorial unless it specifically adresses my question.

## Answer
        
I think the best explanation I've heard was actually fairly recently on SO. `IO Foo` is a recipe for creating a `Foo`. Another common, more literal, way of saying this is that it is a "program that produces a `Foo`". It can be executed (many times) to create a `Foo` or die trying. The execution of the recipe/program is what we ultimately want (otherwise, why write one?), but the thing that is represented by an `IO` action in our code is the recipe itself.

That recipe is a pure value, in the same exact sense that a `String` is a pure value. Recipes can be combined and manipulated in interesting, sometimes astonishing, ways, but the many ways these recipes can be combined (except for the blatantly non-pure `unsafePerformIO`, `unsafeCoerce`, etc.) are all completely referentially transparent, deterministic, and all that nice stuff. The resulting recipe depends in absolutely no way whatsoever on the state of anything other than the recipes that it was built up from.
