
# var functionName = function() {} vs function functionName() {}

## Question
        
I've recently started maintaining someone else's JavaScript code. I'm fixing bugs, adding features and also trying to tidy up the code and make it more consistent.

The previous developer uses two ways of declaring functions and I can't work out if there is a reason behind it or not.

The two ways are:

    var functionOne = function() {
        // Some code
    };
    

    function functionTwo() {
        // Some code
    }
    

What are the reasons for using these two different methods and what are the pros and cons of each? Is there anything that can be done with one method that can't be done with the other?

## Answer
        
The difference is that `functionOne` is a function expression and so only defined when that line is reached, whereas `functionTwo` is a function declaration and is defined as soon as its surrounding function or script is executed (due to [hoisting](http://adripofjavascript.com/blog/drips/variable-and-function-hoisting.html)).

For example, a function expression:

    // TypeError: functionOne is not a function
    functionOne();
    
    var functionOne = function() {
      console.log("Hello!");
    };

And, a function declaration:

    // Outputs: "Hello!"
    functionTwo();
    
    function functionTwo() {
      console.log("Hello!");
    }

This also means you can't conditionally define functions using function declarations:

    if (test) {
       // Error or misbehavior
       function functionThree() { doSomething(); }
    }
    

The above actually defines `functionThree` irrespective of `test`'s value — unless `use strict` is in effect, in which case it simply raises an error.
