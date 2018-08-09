
# Haskell syntax error for where statement [duplicate]

## Question
        
This question already has an answer here:

*   [Why shouldn't I mix tabs and spaces?](/questions/35855170/why-shouldnt-i-mix-tabs-and-spaces) 1 answer

I'm writing some Haskell code to learn the language, and I've run into the syntax error:

`Vec2.hs:33:27: parse error on input '='`

The code I've written here is below. The error is pointing at the 2nd term in `vec2Normalize` `iLength = ...` I don't see the syntax error

    -- Get the inverse length of v and multiply the components by it
    -- Resulting in the normalized form of v
    vec2Normalize :: Vec2 -> Vec2
    vec2Normalize v@(x,y) = (x * iLength, y * iLength)
        where length = vec2Length v
              iLength = if length == 0 then 1 else (1 / length)

## Answer
        
Some guessing involved since you don’t provide the complete code, but this error could indicate that your line `iLength = ...` is not properly indented; actually, that the `iLength` starts to the _right_ of the `length =` on the line before.

Does your original file use tabs instead of spaces for indentation? If so, be aware that Haskell always interprets a tab as spanning 8 columns. So, e.g.,

    <TAB>where length = ...
    <TAB><TAB><SPACE><SPACE>iLength = ...
    

would be interpreted as

            where length = ...
                      iLength = ...
    

thus causing the error, even though your editor might show the lines properly aligned if it uses 4-column tabs.
