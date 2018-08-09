
# What is the purpose of the var keyword and when should I use it (or omit it)?

## Question
        
> **_NOTE_**_: This question was asked from the viewpoint of ECMAScript version 3 or 5. The answers might become outdated with the introduction of new features in the release of ECMAScript 6._

What exactly is the function of the `var` keyword in JavaScript, and what is the difference between

    var someNumber = 2;
    var someFunction = function() { doSomething; }
    var someObject = { }
    var someObject.someProperty = 5;
    

and

    someNumber = 2;
    someFunction = function() { doSomething; }
    someObject = { }
    someObject.someProperty = 5;
    

?

When would you use either one, and why/what does it do?

## Answer
        
If you're in the global scope then there's not much difference. Read [Kangax's](https://stackoverflow.com/a/1471738/1541051) answer for explanation

If you're in a function then **`var`** will create a local variable, "no var" will look up the scope chain until it finds the variable or hits the global scope (at which point it will create it):

    // These are both globals
    var foo = 1;
    bar = 2;
    
    function()
    {
        var foo = 1; // Local
        bar = 2;     // Global
    
        // Execute an anonymous function
        (function()
        {
            var wibble = 1; // Local
            foo = 2; // Inherits from scope above (creating a closure)
            moo = 3; // Global
        }())
    }
    

If you're not doing an assignment then you need to use `var`:

    var x; // Declare x
