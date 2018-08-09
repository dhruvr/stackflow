
# Dot Operator in Haskell: need more explanation

## Question
        
I'm trying to understand what the dot operator is doing in this Haskell code:

    sumEuler = sum . (map euler) . mkList
    

The entire source code is below.

My understanding
----------------

The dot operator is taking the two functions `sum` and the result of `map euler` and the result of `mkList` as the input.

But, `sum` isn't a function it is the argument of the function, right? So what is going on here?

Also, what is `(map euler)` doing?

Code
----

    mkList :: Int -> [Int]
    mkList n = [1..n-1]
    
    euler :: Int -> Int
    euler n = length (filter (relprime n) (mkList n))
    
    sumEuler :: Int -> Int
    sumEuler = sum . (map euler) . mkList

## Answer
        
Put simply, `.` is function composition, just like in math:

    f (g x) = (f . g) x
    

In your case, you are creating a new function, `sumEuler` that could also be defined like this:

    sumEuler x = sum (map euler (mkList x))
    

The style in your example is called "point-free" style -- the arguments to the function are omitted. This makes for clearer code in many cases. (It can be hard to grok the first time you see it, but you will get used to it after a while. It is a common Haskell idiom.)

If you are still confused, it may help to relate `.` to something like a UNIX pipe. If `f`'s output becomes `g`'s input, whose output becomes `h`'s input, you'd write that on the command-line like `f < x | g | h`. In Haskell, `.` works like the UNIX `|`, but "backwards" -- `h . g . f $ x`. I find this notation to be quite helpful when, say, processing a list. Instead of some unwieldy construction like `map (\x -> x * 2 + 10) [1..10]`, you could just write `(+10) . (*2) <$> [1..10]`. (And, if you want to only apply that function to a single value; it's `(+10) . (*2) $ 10`. Consistent!)

The Haskell wiki has a good article with some more detail: [http://www.haskell.org/haskellwiki/Pointfree](http://www.haskell.org/haskellwiki/Pointfree)
