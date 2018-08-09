
# What&apos;s the point of map in Haskell, when there is fmap?

## Question
        
Everywhere I've tried using `map`, `fmap` has worked as well. Why did the creators of Haskell feel the need for a `map` function? Couldn't it just be what is currently known as `fmap` and `fmap` could be removed from the language?

## Answer
        
I would like to make an answer to draw attention to [augustss's comment](https://stackoverflow.com/questions/6824255/whats-the-point-of-map-in-haskell-when-there-is-fmap#comment-8112310):

> That's not actually how it happens. What happened was that the type of map was generalized to cover Functor in Haskell 1.3. I.e., in Haskell 1.3 fmap was called map. This change was then reverted in Haskell 1.4 and fmap was introduced. The reason for this change was pedagogical; when teaching Haskell to beginners the very general type of map made error messages more difficult to understand. In my opinion this wasn't the right way to solve the problem.

Haskell 98 is seen as a step backwards by some Haskellers (including me), previous versions having defined a more abstract and consistent library. Oh well.
