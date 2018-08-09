
# Why doesn&apos;t TypeSynonymInstances allow partially applied type synonyms to be used in instance heads?

## Question
        
I know that [TypeSynomymInstances only allows fully applied type synonyms to be used in instance heads](http://hackage.haskell.org/trac/haskell-prime/wiki/TypeSynonymInstances), but it seems like it would be handy if I could use paritally applied type synonyms to be used as well.

For instance:

    class Example e where
      thingy :: a -> b -> e a b
    
    -- legit, but awkward
    newtype FuncWrapper e a b = FuncWrapper { ap :: a -> e a b }
    instance (Example e) => Example (FuncWrapper e) where
      thingy _ = FuncWrapper . flip thingy
    funcWrapperUse :: (Example e) => e Int String
    funcWrapperUse = thingy 1 "two" `ap` 3 `ap` 4 `ap` 5
    
    -- not legal, but a little easier to use
    type FuncSynonym e a b = a -> e a b
    instance (Example e) => Example (FuncSynonym e) where
      thingy _ = flip thingy
    funcSynonymUse :: (Example e) => e Int String
    funcSynonymUse = thingy 1 "two" 3 4 5

## Answer
        
Partially applied type synonyms are not allowed in Haskell at all. A partially applied synonym is effectively a function whose inputs are the un-applied types and whose output is a type. For example, here is an encoding of boolean logic:

    type True x y = x
    type False x y = y
    type Not b x y = b y x
    type And b1 b2 x y = b1 (b2 x y) y
    type Or b1 b2 x y = b1 x (b2 x y)
    

To decide whether two partially applied type synonyms are equal, the type checker would have to decide whether functions are equal. This is a hard problem, and in general it is undecidable.
