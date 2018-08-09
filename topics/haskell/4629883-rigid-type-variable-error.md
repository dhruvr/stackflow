
# rigid type variable error

## Question
        
What is wrong with this function ?

    test :: Show s ⇒ s
    test = "asdasd"
    

String is an instance of Show class, so seems correct.

The error is

    src\Main.hs:224:7:
        Couldn't match expected type `s' against inferred type `[Char]'
          `s' is a rigid type variable bound by
              the type signature for `test' at src\Main.hs:223:13
        In the expression: "asdasd"
        In the definition of `test': test = "asdasd"

## Answer
        
`test :: Foo a => a` means "for any type which is an instance of `Foo`, `test` is a value of that type". So in any place where you can use a value of type `X` where `X` is an instance `Foo`, you can use a value of type `Foo a => a`.

Something like `test :: Num a => a; test = 42` works because 42 can be a value of type `Int` or `Integer` or `Float` or anything else that is an instance of `Num`.

However `"asdasd"` can't be an `Int` or anything else that is an instance of `Show` \- it can only ever be a `String`. As a consequence it does not match the type `Show s => s`.
