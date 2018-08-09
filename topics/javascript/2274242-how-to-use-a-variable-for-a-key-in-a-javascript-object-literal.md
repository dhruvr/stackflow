
# How to use a variable for a key in a JavaScript object literal?

## Question
        
Why does the following work?

    <something>.stop().animate(
        { 'top' : 10 }, 10
    );
    

Whereas this doesn't work:

    var thetop = 'top';
    <something>.stop().animate(
        { thetop : 10 }, 10
    );
    

To make it even clearer: At the moment I'm not able to pass a CSS property to the animate function as a variable.

## Answer
        
`{ thetop : 10 }` is a valid object literal. The code will create an object with a property named `thetop` that has a value of 10. Both the following are the same:

    obj = { thetop : 10 };
    obj = { "thetop" : 10 };
    

In ES5 and earlier, you cannot use a variable as a property name inside an object literal. Your only option is to do the following:

    var thetop = "top";
    
    // create the object literal
    var aniArgs = {};
    
    // Assign the variable property name with a value of 10
    aniArgs[thetop] = 10; 
    
    // Pass the resulting object to the animate method
    <something>.stop().animate(
        aniArgs, 10  
    );  
    

ES6 [defines](http://www.ecma-international.org/ecma-262/6.0/#sec-object-initializer) _ComputedPropertyName_ as part of the grammar for object literals, which allows you to write the code like this:

    var thetop = "top",
        obj = { [thetop]: 10 };
    
    console.log(obj.top); // -> 10
    

You can use this new syntax in the latest versions of each mainstream browser.
