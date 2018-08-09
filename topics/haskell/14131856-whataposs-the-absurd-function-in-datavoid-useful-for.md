
# What&apos;s the absurd function in Data.Void useful for?

## Question
        
The [`absurd`](http://hackage.haskell.org/packages/archive/void/0.5.10/doc/html/Data-Void.html#v:absurd) function in `Data.Void` has the following signature, where `Void` is the logically uninhabited type exported by that package:

    -- | Since 'Void' values logically don't exist, this witnesses the logical
    -- reasoning tool of \"ex falso quodlibet\".
    absurd :: Void -> a
    

I do know enough logic to get the documentation's remark that this corresponds, by the propositions-as-types correspondence, to the valid formula `⊥ → a`.

What I'm puzzled and curious about is: in what sort of practical programming problems is this function useful? I'm thinking that perhaps it's useful in some cases as a type-safe way of exhaustively handling "can't happen" cases, but I don't know enough about practical uses of Curry-Howard to tell whether that idea is on the right track at all.

EDIT: Examples preferably in Haskell, but if anybody wants to use a dependently typed language I'm not going to complain...

## Answer
        
Life is a little bit hard, since Haskell is non strict. The general use case is to handle impossible paths. For example

    simple :: Either Void a -> a
    simple (Left x) = absurd x
    simple (Right y) = y
    

This turns out to be somewhat useful. Consider a simple type for `Pipes`

    data Pipe a b r
      = Pure r
      | Await (a -> Pipe a b r)
      | Yield !b (Pipe a b r)
    

this is a strict-ified and simplified version of the standard pipes type from Gabriel Gonzales' `Pipes` library. Now, we can encode a pipe that never yields (ie, a consumer) as

    type Consumer a r = Pipe a Void r
    

this really never yields. The implication of this is that the proper fold rule for a `Consumer` is

    foldConsumer :: (r -> s) -> ((a -> s) -> s) -> Consumer a r -> s
    foldConsumer onPure onAwait p 
     = case p of
         Pure x -> onPure x
         Await f -> onAwait $ \x -> foldConsumer onPure onAwait (f x)
         Yield x _ -> absurd x
    

or alternatively, that you can **ignore** the yield case when dealing with consumers. This is the general version of this design pattern: use polymorphic data types and `Void` to get rid of possibilities when you need to.

Probably the most classic use of `Void` is in CPS.

    type Continuation a = a -> Void
    

that is, a `Continuation` is a function which never returns. `Continuation` is the type version of "not." From this we get a monad of CPS (corresponding to classical logic)

    newtype CPS a = Continuation (Continuation a)
    

since Haskell is pure, we can't get anything out of this type.
