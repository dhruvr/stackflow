
# What does the : infix operator do in Haskell?

## Question
        
I'm reading [A Gentle Introduction to Haskell](http://www.haskell.org/tutorial/index.html) (which is not so gentle) and it repeatedly uses the `:` operator without directly explaining what it does.

So, what exactly does it do?

## Answer
        
`:` is the “prepend” operator:

    x : xs
    

Returns a list which has `x` as first element, followed by all elements in `xs`. In other functional languages, this is usually called `cons`, because it “cons”tructs a list recursively by repeated application from an empty list:

    1 : 2 : 3 : 4 : []
    

is the list `[1, 2, 3, 4]`.
