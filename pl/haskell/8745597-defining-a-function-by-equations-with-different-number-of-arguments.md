
# Defining a function by equations with different number of arguments

## Question
      
I noticed today that such a definition

    safeDivide x 0 = x
    safeDivide = (/)
    

is not possible. I am just curious what the (good) reason behind this is. There must be a very good one (it's Haskell after all :)).

Note: I am not looking suggestions for alternative implementations to the code above, it's a simple example to demonstrate my point.
## Answer
      
I think it's mainly for consistency so that all clauses can be read in the same manner, so to speak; i.e. every RHS is at the same position in the type of the function. I think would mask quite a few silly errors if you allowed this, too.

There's also a slight semantic quirk: say the compiler padded out such clauses to have the same number of patterns as the other clauses; i.e. your example would become

    safeDivide x 0 = x
    safeDivide x y = (/) x y
    

Now consider if the second line had instead been `safeDivide = undefined`; in the absence of the previous clause, `safeDivide` would be `⊥`, but thanks to the eta-expansion performed here, it's `\x y -> if y == 0 then x else ⊥` — so `safeDivide = undefined` does not actually define `safeDivide` to be `⊥`! This seems confusing enough to justify banning such clauses, IMO.
    