
# Dynamically access object property using variable

## Question
        
I'm trying to access a property of an object using a dynamic name. Is this possible?

    const something = { bar: "Foobar!" };
    const foo = 'bar';
    something.foo; // The idea is to access something.bar, getting "Foobar!"

## Answer
        
There are [two ways to access properties](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Property_Accessors) of an object:

*   Dot notation: `something.bar`
*   Bracket notation: `something['bar']`

The value between the brackets can be any expression. Therefore, if the property name is stored in a variable, you have to use bracket notation:

    var foo = 'bar';
    something[foo];
    // both x = something[foo] and something[foo] = x work as expected
