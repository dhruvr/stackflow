
# Haskell: Unexpected output for expression [0, 0.1 .. 1]

## Question
        
When evaluating the expression:

    *main> [0, 0.1 .. 1]
    

I was actually expecting:

     [0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1]
    

But I was quite shocked to see the output be

    [0.0,0.1,0.2,0.30000000000000004,0.4000000000000001,0.5000000000000001,0.6000000000000001,0.7000000000000001,0.8,0.9,1.0]
    

_**Why does Haskell produce that result upon evaluation?**_

## Answer
        
This is a result of the imprecision of [floating point](http://en.wikipedia.org/wiki/Floating_point) values, it isn't particular to Haskell. If you can't deal with the approximation inherent in floating point then you can use [Rational](http://hackage.haskell.org/packages/archive/base/latest/doc/html/Prelude.html#t%3aRational) at a high performance cost:

    > import Data.Ratio
    Data.Ratio> [0,1%10.. 1%1]
    [0 % 1,1 % 10,1 % 5,3 % 10,2 % 5,1 % 2,3 % 5,7 % 10,4 % 5,9 % 10,1 % 1]
    

Just to hammer the point home, here's Python:

    >>> 0.3
    0.29999999999999999
    

And here's C:

    void main() { printf("%0.17f\n",0.3); }
    
    $ gcc t.c 2>/dev/null ; ./a.out
    0.29999999999999999
