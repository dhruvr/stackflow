
# How to deal with floating point number precision in JavaScript?

## Question
        
I have the following dummy test script:

    function test(){
        var x = 0.1 * 0.2;
        document.write(x);
    }
    test();
    

This will print the result `0.020000000000000004` while it should just print `0.02` (if you use your calculator). As far as I understood this is due to errors in the floating point multiplication precision.

Does anyone have a good solution so that in such case I get the correct result `0.02`? I know there are functions like `toFixed` or rounding would be another possibility, but I'd like to really have the whole number printed without any cutting and rounding. Just wanted to know if one of you has some nice, elegant solution.

Of course, otherwise I'll round to some 10 digits or so.

## Answer
        
From the [Floating-Point Guide](http://floating-point-gui.de/):

> **What can I do to avoid this problem?**
> 
> That depends on what kind of calculations you’re doing.
> 
> *   If you really need your results to add up exactly, especially when you work with money: use a special decimal datatype.
> *   If you just don’t want to see all those extra decimal places: simply format your result rounded to a fixed number of decimal places when displaying it.
> *   If you have no decimal datatype available, an alternative is to work with integers, e.g. do money calculations entirely in cents. But this is more work and has some drawbacks.

Note that the first point only applies if you really need specific precise _decimal_ behaviour. Most people don't need that, they're just irritated that their programs don't work correctly with numbers like 1/10 without realizing that they wouldn't even blink at the same error if it occurred with 1/3.

If the first point really applies to you, use [BigDecimal for JavaScript](https://github.com/dtrebbien/BigDecimal.js), which is not elegant at all, but actually solves the problem rather than providing an imperfect workaround.
