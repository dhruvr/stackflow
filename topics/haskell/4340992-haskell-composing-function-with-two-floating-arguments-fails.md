
# Haskell: composing function with two floating arguments fails

## Question
        
I am trying to compose a function of type `(Floating a) => a -> a -> a` with a function of type `(Floating a) => a -> a` to obtain a function of type `(Floating a) => a -> a -> a`. I have the following code:

    test1 :: (Floating a) => a -> a -> a
    test1 x y = x
    
    test2 :: (Floating a) => a -> a
    test2 x = x
    
    testBoth :: (Floating a) => a -> a -> a
    testBoth = test2 . test1
    --testBoth x y = test2 (test1 x y)
    

However, when I compile it in GHCI, I get the following error:

    /path/test.hs:8:11:
        Could not deduce (Floating (a -> a)) from the context (Floating a)
          arising from a use of `test2'
                       at /path/test.hs:8:11-15
        Possible fix:
          add (Floating (a -> a)) to the context of
            the type signature for `testBoth'
          or add an instance declaration for (Floating (a -> a))
        In the first argument of `(.)', namely `test2'
        In the expression: test2 . test1
        In the definition of `testBoth': testBoth = test2 . test1
    Failed, modules loaded: none.
    

Note that the commented-out version of `testBoth` compiles. The strange thing is that if I remove the `(Floating a)` constraints from all type signatures or if I change `test1` to just take `x` instead of `x` and `y`, `testBoth` compiles.

I've searched StackOverflow, Haskell wikis, Google, etc. and not found anything about a restriction on function composition relevant to this particular situation. Does anyone know why this is happening?

## Answer
        
       \x y -> test2 (test1 x y)
    == \x y -> test2 ((test1 x) y)
    == \x y -> (test2 . (test1 x)) y
    == \x -> test2 . (test1 x)
    == \x -> (test2 .) (test1 x)
    == \x -> ((test2 .) . test1) x
    == (test2 .) . test1
    

These two things are not like each other.

       test2 . test1
    == \x -> (test2 . test1) x
    == \x -> test2 (test1 x)
    == \x y -> (test2 (test1 x)) y
    == \x y -> test2 (test1 x) y
