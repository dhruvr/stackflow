
# Convert String to Integer/Float in Haskell?

## Question
        
    data GroceryItem = CartItem ItemName Price Quantity | StockItem ItemName Price Quantity
    
    makeGroceryItem :: String -> Float -> Int -> GroceryItem
    makeGroceryItem name price quantity = CartItem name price quantity
    
    I want to create a `GroceryItem` when using a `String` or `[String]`
    
    createGroceryItem :: [String] -> GroceryItem
    createGroceryItem (a:b:c) = makeGroceryItem a b c
    

The input will be in the format `["Apple","15.00","5"]` which I broke up using Haskell's `words` function.

I get the following error which I think is because `makeGroceryItem` accepts a `Float` and an `Int`.

    *Type error in application
    *** Expression     : makeGroceryItem a read b read c
    *** Term           : makeGroceryItem
    *** Type           : String -> Float -> Int -> GroceryItem
    *** Does not match : a -> b -> c -> d -> e -> f*
    

But how do I make `b` and `c` of type `Float` and `Int`, respectively?

## Answer
        
`read` can parse a string into float and int:

    Prelude> :set +t
    Prelude> read "123.456" :: Float
    123.456
    it :: Float
    Prelude> read "123456" :: Int
    123456
    it :: Int
    

But the problem (1) is in your pattern:

    createGroceryItem (a:b:c) = ...
    

Here `:` is a (right-associative) binary operator which prepends an element to a list. The RHS of an element must be a list. Therefore, given the expression `a:b:c`, Haskell will infer the following types:

    a :: String
    b :: String
    c :: [String]
    

i.e. `c` will be thought as a list of strings. Obviously it can't be `read` or passed into any functions expecting a String.

Instead you should use

    createGroceryItem [a, b, c] = ...
    

if the list must have exactly 3 items, or

    createGroceryItem (a:b:c:xs) = ...
    

if ≥3 items is acceptable.

Also (2), the expression

    makeGroceryItem a read b read c
    

will be interpreted as `makeGroceryItem` taking 5 arguments, 2 of which are the `read` function. You need to use parenthesis:

    makeGroceryItem a (read b) (read c)
