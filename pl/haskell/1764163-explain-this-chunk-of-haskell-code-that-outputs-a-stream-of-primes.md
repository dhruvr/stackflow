
# Explain this chunk of haskell code that outputs a stream of primes

## Question
      
I have trouble understanding this chunk of code:

    let
      sieve (p:xs) = p : sieve (filter (\ x -> x `mod` p /= 0) xs)
    in sieve [2 .. ]
    

Can someone break it down for me? I understand there is recursion in it, but thats the problem I can't understand how the recursion in this example works.
## Answer
      
It's actually pretty elegant.

First, we define a function `sieve` that takes a list of elements:

    sieve (p:xs) =
    

In the body of `sieve`, we take the head of the list (because we're passing the infinite list `[2..]`, and 2 is defined to be prime) and append it (lazily!) to the result of applying `sieve` to the rest of the list:

    p : sieve (filter (\ x -> x 'mod' p /= 0) xs)
    

So let's look at the code that does the work on the rest of the list:

    sieve (filter (\ x -> x 'mod' p /= 0) xs)
    

We're applying `sieve` to the filtered list. Let's break down what the filter part does:

    filter (\ x -> x 'mod' p /= 0) xs
    

`filter` takes a function and a list on which we apply that function, and retains elements that meet the criteria given by the function. In this case, `filter` takes an anonymous function:

    \ x -> x 'mod' p /= 0
    

This anonymous function takes one argument, `x`. It checks the _modulus_ of `x` against `p` (the head of the list, every time `sieve` is called):

     x 'mod' p
    

If the modulus is not equal to 0:

     x 'mod' p /= 0
    

Then the element `x` is kept in the list. If it is equal to 0, it's filtered out. This makes sense: if `x` is divisible by `p`, than `x` is divisible by more than just 1 and itself, and thus it is not prime.
    