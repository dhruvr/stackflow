
# Floating point math in different programming languages

## Question
        
I know that floating point math can be ugly at best but I am wondering if somebody can explain the following quirk. In most of the programing languages I tested the addition of 0.4 to 0.2 gave a slight error, where as 0.4 + 0.1 + 0.1 gave non.

What is the reason for the inequality of both calculation and what measures can one undertake in the respective programing languages to obtain correct results.

In python2/3

    .4 + .2
    0.6000000000000001
    .4 + .1 + .1
    0.6
    

The same happens in Julia 0.3

    julia> .4 + .2
    0.6000000000000001
    
    julia> .4 + .1 + .1
    0.6
    

and Scala:

    scala> 0.4 + 0.2
    res0: Double = 0.6000000000000001
    
    scala> 0.4 + 0.1 + 0.1
    res1: Double = 0.6
    

and Haskell:

    Prelude> 0.4 + 0.2
    0.6000000000000001    
    Prelude> 0.4 + 0.1 + 0.1
    0.6
    

but R v3 gets it right:

    > .4 + .2
    [1] 0.6
    > .4 + .1 + .1
    [1] 0.6

## Answer
        
All these languages are using the system-provided floating-point format, which represents values in _binary_ rather than in _decimal_. Values like `0.2` and `0.4` can't be represented exactly in that format, so instead the closest representable value is stored, resulting in a small error. For example, the numeric literal `0.2` results in a floating-point number whose exact value is `0.200000000000000011102230246251565404236316680908203125`. Similarly, any given arithmetic operation on floating-point numbers may result in a value that's not exactly representable, so the true mathematical result is replaced with the closest representable value. These are the fundamental reasons for the errors you're seeing.

However, this doesn't explain the differences between languages: in all of your examples, the exact same computations are being made and the exact same results are being arrived at. The difference then lies in the way that the various languages choose to _display_ the results.

Strictly speaking, _none_ of the answers you show is correct. Making the (fairly safe) assumption of IEEE 754 binary 64 arithmetic with a round-to-nearest rounding mode, the exact value of the first sum is:

    0.600000000000000088817841970012523233890533447265625
    

while the exact value of the second sum is:

    0.59999999999999997779553950749686919152736663818359375
    

However, neither of those outputs is particularly user-friendly, and clearly all of the languages you tested made the sensible decision to abbreviate the output when printing. However, they don't all adopt the same strategy for formatting the output, which is why you're seeing differences.

There are many possible strategies for formatting, but three particularly common ones are:

1.  Compute and display 17 correctly-rounded significant digits, possibly stripping trailing zeros where they appear. The output of 17 digits guarantees that distinct binary64 floats will have distinct representations, so that a floating-point value can be unambiguously recovered from its representation; 17 is the smallest integer with this property. This is the strategy that Python 2.6 uses, for example.
    
2.  Compute and display the shortest decimal string that rounds back to the given binary64 value under the usual round-ties-to-even rounding mode. This is rather more complicated to implement than strategy 1, but preserves the property that distinct floats have distinct representations, and tends to make for pleasanter output. This appears to be the strategy that all of the languages you tested (besides R) are using.
    
3.  Compute and display 15 (or fewer) correctly-rounded significant digits. This has the effect of hiding the errors involved in the decimal-to-binary conversions, giving the illusion of exact decimal arithmetic. It has the drawback that distinct floats can have the same representation. This appears to be what R is doing. (Thanks to @hadley for pointing out in the comments that there's an [R setting](http://stat.ethz.ch/R-manual/R-patched/library/base/html/options.html) which controls the number of digits used for display; the default is to use 7 significant digits.)
