
# What is JavaScript&apos;s highest integer value that a number can go to without losing precision?

## Question
        
Is this defined by the language? Is there a defined maximum? Is it different in different browsers?

## Answer
        
**\+/\- 9007199254740991**

[ECMA Section 8.5 - Numbers](http://ecma262-5.com/ELS5_HTML.htm#Section_8.5)

> Note that all the positive and negative integers whose magnitude is no greater than 253 are representable in the Number type (indeed, the integer 0 has two representations, +0 and −0).

They are 64-bit floating point values, the largest exact integral value is 253-1, or `9007199254740991`. In ES6, this is defined as [Number.MAX\_SAFE\_INTEGER](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Number/MAX_SAFE_INTEGER).

Note that the bitwise operators and shift operators operate on 32-bit ints, so in that case, the max safe integer is 231-1, or 2147483647.

* * *

Test it out!

    var x = 9007199254740992;
    var y = -x;
    x == x + 1; // true !
    y == y - 1; // also true !
    // Arithmetic operators work, but bitwise/shifts only operate on int32:
    x / 2;      // 4503599627370496
    x >> 1;     // 0
    x | 1;      // 1
