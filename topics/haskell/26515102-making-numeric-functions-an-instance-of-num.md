
# Making numeric functions an instance of Num?

## Question
        
I want to be able to compose numeric functions in haskell using binary operators. So, for example, with unary numeric functions:

    f*g
    

should translate to:

    \x -> (f x)*(g x)
    

and similarly for addition. Making your own operator to do this is pretty straightforward, but I'd really like to just make `Num a => a -> a` functions an instance of Num, but I'm not sure how to do so.

I'd also like to make this arity generic, but that might be too much trouble for how difficult it is to do arity generic functions in Haskell, so it might just be better to define seperate `Num a => a -> a -> a`, `Num a => a -> a -> a -> a`, etc... instances up to some reasonably large number.

## Answer
        
an instance with generic arity

    instance Num b => Num (a->b) where
        f + g = \x -> f x + g x
        f - g = \x -> f x - g x
        f * g = \x -> f x * g x
        negate f = negate . f
        abs f = abs . f
        signum f = signum . f
        fromInteger n = \x -> fromInteger n
    

Edit: As Christian Conkle points out, there are problems with this approach. If you plan to use these instances for anything important or just want to understand the issues, you should read the resources he provided and decide for yourself whether this fits your needs. My intention was to provide an easy way to play with numeric functions using natural notation with as simple an implementation as possible.
