
# Why does ghci say that 1.1 + 1.1 + 1.1 &gt; 3.3 is True?

## Question
        
I've been going through a Haskell tutorial recently and noticed this behaviour when trying some simple Haskell expressions in the interactive `ghci` shell:

    Prelude> 1.1 + 1.1 == 2.2
    True
    Prelude> 1.1 + 1.1 + 1.1 == 3.3
    False
    Prelude> 1.1 + 1.1 + 1.1 > 3.3
    True
    Prelude> 1.1 + 1.1 + 1.1
    3.3000000000000003
    

Does anybody know why that is?

## Answer
        
Because `1.1` and `3.3` are [floating point numbers](http://en.wikipedia.org/wiki/Floating_point). Decimal fractions, such as .1 or .3, are not exactly representable in a binary floating point number. .1 means 1/10. To represent that in binary, where each fractional digit represents 1/2n (1/2, 1/4, 1/8, etc), you would need an infinite number of digits, 0.000110011... repeating infinitely.

This is exactly the same problem as representing, say, 1/3 in base 10. In base 10, you would need an infinite number of digits, .33333... forever, to represent 1/3 exactly. So working in base 10, you usually round, to something like .33. But if you add up three copies of that, you get .99, not 1.

For far more information on the topic, read [What Every Computer Scientist Should Know About Floating Point Arithmetic](http://perso.ens-lyon.fr/jean-michel.muller/goldberg.pdf).

For representing rational numbers more precisely in Haskell, you can always use the rational data type, [`Ratio`](http://www.haskell.org/onlinereport/ratio.html); coupled with bignums (arbitrarily large integers, `Integer` in Haskell, as opposed to `Int` which are fixed size) as the type for numerator and denominator, you can represent arbitrarily precise rational numbers, but at a significantly slower speed than floating point numbers, which are implemented in hardware and optimized for speed.

Floating point numbers are an optimization, for scientific and numerical computation, that trade off precision for high speed, allowing you to perform a very large number of computations in a small time, as long as you are aware of rounding and how it affects your computations.
