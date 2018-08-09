
# Kind Signatures

## Question
        
I am going through the Haskell wiki books GADTS

[https://en.wikibooks.org/wiki/Haskell/GADT](https://en.wikibooks.org/wiki/Haskell/GADT) guide.

I was tracking pretty well until a Kind signature was added which generalizes the constrained type of the Cons constructor.

    data Safe
    data NotSafe
    
    data MarkedList             ::  * -> * -> * where
      Nil                       ::  MarkedList t NotSafe
      Cons                      ::  a -> MarkedList a b -> MarkedList a c
    
    safeHead                    ::  MarkedList a Safe -> a
    safeHead (Cons x _)          =  x
    
    
    silly 0                      =  Nil
    silly 1                      =  Cons () Nil
    silly n                      =  Cons () $ silly (n-1)
    

With the Kind Signature I can use the Cons constructor to construct and pattern match against both Safe and Unsafe MarkedLists. While I understand what going on I am unfortunately having trouble building any intuition as to how the Kind Signature is allowing this. Why do I need the Kind Signature? What is the Kind Signature doing?

## Answer
        
The same way a type signature works for values, a kind signature works for types.

    f :: Int -> Int -> Bool
    f x y = x < y
    

Here, `f` takes two argument values and produces a result value. The equivalent for types could be:

    data D a b = D a b
    

The type `D` takes two argument types and produces a result type (it is `* -> * -> *`). For example, `D Int String` is a type (which has kind `*`). The partial application `D Int` has kind `* -> *`, just the same way the partial application `f 15` has type `Int -> Bool`.

So we could rewrite the above as:

    data D :: * -> * -> * where
      D :: a -> b -> D a b
    

In GHCi, you can query types and kinds:

    > :type f
    f :: Int -> Int -> Bool
    > :kind D
    D :: * -> * -> *
