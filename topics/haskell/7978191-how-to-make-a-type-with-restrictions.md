
# How to make a type with restrictions

## Question
        
For example I want to make a type MyType of integer triples. But not just Cartesian product of three Integer, I want the type to represent all (x, y, z) such that x + y + z = 5

How do I do that? Except of using just (x, y) since z = 5 - x - y

And the same question if I have three constructors A, B, C and the type should be all (A x, B y, C z) such that x + y + z = 5

## Answer
        
I think the trick here is that you don't enforce it on the type-level, you use "smart constructors": i.e. only allow creation of such "tuples" via a function that generates such values:

    module Test(MyType,x,y,z,createMyType) where
    
    data MyType = MT { x :: Int, y :: Int, z :: Int }
    
    createMyType :: Int -> Int -> MyType
    createMyType myX myY = MT { x = myX, y = myY, z = 5 - myX - myY }
    

If you want to generate all possible such values, then you can write a function to do so, either with provided or specified bounds.

It may very well be possible to use type-level Church Numerals or some such so as to enforce creation of these, but it's almost definitely too much work for what you probably want/need.

This might not be what you want (i.e. "Except of using just (x, y) since z = 5 - x - y") but it makes more sense than trying to have some kind of enforced restriction on the type level for allowing valid values.

Types can ensure the correct "type" of value (no pun intended); to ensure validity of values you hide the constructor and only allow creation via approved functions that guarantee any invariants you require.
