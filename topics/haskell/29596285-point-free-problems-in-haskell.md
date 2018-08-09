
# Point Free problems in Haskell

## Question
        
I am trying to convert the following haskell code to point free style, with no avail.

    bar f g xs = filter f (map g xs )
    

I am new to haskell and any help would be great

## Answer
        
Converting to pointfree style can be done entirely mechanically, though it's hard without being comfortable with the fundamentals of Haskell syntax like left-associative function application and `x + y` being the same as `(+) x y`. I will assume you are comfortable with Haskell syntax; if not, I suggest going through the first few chapters of [LYAH](http://learnyouahaskell.com/) first.

You need the following combinators, which are in the standard library. I have also given their standard names from combinator calculus.

    id :: a -> a                                   -- I
    const :: a -> b -> a                           -- K
    (.) :: (b -> c) -> (a -> b) -> (a -> c)        -- B
    flip :: (a -> b -> c) -> (b -> a -> c)         -- C
    (<*>) :: (a -> b -> c) -> (a -> b) -> (a -> c) -- S
    

Work with one parameter at a time. Move parameters on the left to lambdas on the right, e.g.

    f x y = Z
    

becomes

    f = \x -> \y -> Z
    

I like to do this one argument at a time rather than all at once, it just looks cleaner.

Then eliminate the lambda you just created according to the following rules. I will use lowercase letters for literal variables, uppercase letters to denote more complex expressions.

1.  If you have `\x -> x`, replace with `id`
2.  If you have `\x -> A`, where `A` is any expression in which `x` does not occur, replace with `const A`
3.  If you have `\x -> A x`, where `x` does not occur in `A`, replace with `A`. This is known as "eta contraction".
4.  If you have `\x -> A B`, then
    1.  If `x` occurs in both `A` and `B`, replace with `(\x -> A) <*> (\x -> B)`.
    2.  If `x` occurs in just `A`, replace with `flip (\x -> A) B`
    3.  If `x` occurs in just `B`, replace with `A . (\x -> B)`,
    4.  If `x` does not occur in either `A` or `B`, well, there's another rule we should have used already.

And then work inward, eliminating the lambdas that you created. Lets work with this example:

    f x y z = foo z (bar x y)
    -- Move parameter to lambda:
    f x y = \z -> foo z (bar x y)
    -- Remember that application is left-associative, so this is the same as
    f x y = \z -> (foo z) (bar x y)
    -- z appears on the left and not on the right, use flip
    f x y = flip (\z -> foo z) (bar x y)
    -- Use rule (3) 
    f x y = flip foo (bar x y)
    
    -- Next parameter
    f x = \y -> flip foo (bar x y)
    -- Application is left-associative
    f x = \y -> (flip foo) (bar x y)
    -- y occurs on the right but not the left, use (.)
    f x = flip foo . (\y -> bar x y)
    -- Use rule 3
    f x = flip foo . bar x
    
    -- Next parameter
    f = \x -> flip foo . bar x
    -- We need to rewrite this operator into normal application style
    f = \x -> (.) (flip foo) (bar x)
    -- Application is left-associative
    f = \x -> ((.) (flip foo)) (bar x)
    -- x appears on the right but not the left, use (.)
    f = ((.) (flip foo)) . (\x -> bar x)
    -- use rule (3)
    f = ((.) (flip foo)) . bar
    -- Redundant parentheses
    f = (.) (flip foo) . bar
    

There you go, now try it on yours! There is not really any cleverness involved in deciding which rule to use: use any rule that applies and you will make progress.
