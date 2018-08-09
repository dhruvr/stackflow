
# Is there any connection between `a :~: b` and `(a :== b) :~: True`?

## Question
        
Is there any connection implemented between [propositional](https://hackage.haskell.org/package/base-4.9.0.0/docs/Data-Type-Equality.html) and [promoted](https://hackage.haskell.org/package/singletons-2.2/docs/Data-Singletons-Prelude-Eq.html) equality?

Let's say I have

    prf :: x :~: y
    

in scope for some `Symbol`s; by pattern matching on it being `Refl`, I can transform that into

    prf' :: (x :== y) :~: True
    

like this:

    fromProp :: (KnownSymbol x, KnownSymbol y) => x :~: y -> (x :== y) :~: True
    fromProp Refl = Refl
    

But what about the other direction? If I try

    toProp :: (KnownSymbol x, KnownSymbol y) => (x :== y) :~: True -> x :~: y
    toProp Refl = Refl
    

then all I get is

    • Could not deduce: x ~ y
      from the context: 'True ~ (x :== y)

## Answer
        
Yes, going between the two representations is possible (assuming the implementation of `:==` is correct), but it requires computation.

The information you need is not present in the Boolean itself ([it's been erased to a single bit](https://existentialtype.wordpress.com/2011/03/15/boolean-blindness/)); you have to recover it. This involves interrogating the two participants of the original Boolean equality test (which means you have to keep them around at runtime), and using your knowledge of the result to eliminate the impossible cases. It's rather tedious to re-perform a computation for which you already know the answer!

Working in Agda, and using naturals instead of strings (because they're simpler):

    open import Data.Nat
    open import Relation.Binary.PropositionalEquality
    open import Data.Bool
    
    _==_ : ℕ -> ℕ -> Bool
    zero == zero = true
    suc n == suc m = n == m
    _ == _ = false
    
    ==-refl : forall n -> (n == n) ≡ true
    ==-refl zero = refl
    ==-refl (suc n) = ==-refl n
    
    
    fromProp : forall {n m} -> n ≡ m -> (n == m) ≡ true
    fromProp {n} refl = ==-refl n
    
    -- we have ways of making you talk
    toProp : forall {n m} -> (n == m) ≡ true -> n ≡ m
    toProp {zero} {zero} refl = refl
    toProp {zero} {suc m} ()
    toProp {suc n} {zero} ()
    toProp {suc n} {suc m} p = cong suc (toProp {n}{m} p)
    

In principle I think you could make this work in Haskell using singletons, but why bother? Don't use Booleans!
