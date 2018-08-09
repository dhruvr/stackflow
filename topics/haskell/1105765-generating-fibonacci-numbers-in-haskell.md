
# Generating Fibonacci numbers in Haskell?

## Question
        
In Haskell, how can I generate Fibonacci numbers based on the property that the nth Fibonacci number is equal to the (n-2)th Fibonacci number plus the (n-1)th Fibonacci number?

I've seen this:

    fibs :: [Integer]
    fibs = 1 : 1 : zipWith (+) fibs (tail fibs)
    

I don't really understand that, or how it produces an infinite list instead of one containing 3 elements.

How would I write haskell code that works by calculating the actual definition and not by doing something really weird with list functions?

## Answer
        
Here's a simple function that calculates the n'th Fibonacci number:

    fib :: Integer -> Integer
    fib 0 = 0
    fib 1 = 1
    fib n = fib (n-1) + fib (n-2)
    

The function in your question works like this:

Assume you already had an infinite list of the Fibonacci numbers:

       [ 1, 1, 2, 3, 5,  8, 13, .... ]
    

The `tail` of this list is

       [ 1, 2, 3, 5, 8, 13, 21, .... ]
    

`zipWith` combines two lists element by element using the given operator:

       [ 1, 1, 2, 3,  5,  8, 13, .... ]
    +  [ 1, 2, 3, 5,  8, 13, 21, .... ]
    =  [ 2, 3, 5, 8, 13, 21, 34, .... ]
    

So the infinite list of Fibonacci numbers can be calculated by prepending the elements `1` and `1` to the result of zipping the infinite list of Fibonacci numbers with the tail of the infinite list of Fibonacci numbers using the `+` operator.

Now, to get the n'th Fibonacci number, just get the n'th element of the infinite list of Fibonacci numbers:

    fib n = fibs !! n
    

The beauty of Haskell is that it doesn't calculate any element of the list of Fibonacci numbers until its needed.

Did I make your head explode? :)
