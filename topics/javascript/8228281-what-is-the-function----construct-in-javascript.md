
# What is the (function() { } )() construct in JavaScript?

## Question
        
I used to know what this meant, but I'm struggling now...

Is this basically saying `document.onload`?

    (function () {
    
    })();

## Answer
        
It’s an [Immediately-Invoked Function Expression](http://benalman.com/news/2010/11/immediately-invoked-function-expression/), or [**IIFE**](https://en.wikipedia.org/wiki/Immediately-invoked_function_expression) for short. It executes immediately after it’s created.

It has nothing to do with any event-handler for any events (such as `document.onload`).  
Consider the part within the first pair of parentheses: `(**function(){}**)();`....it is a regular function declaration. Then look at the last pair `(function(){})**()**;`, this is normally added to an expression to call a function; in this case, our prior expression.

This pattern is often used when trying to avoid polluting the global namespace, because all the variables used inside the IIFE (like in any other _normal_ function) are not visible outside its scope.  
This is why, maybe, you confused this construction with an event-handler for `window.onload`, because it’s often used as this:

    (function(){
        // all your code here
        var foo = function() {};
        window.onload = foo;
        // ...
    })();
    // foo is unreachable here (it’s undefined)
    

**Correction suggested by [Guffa](https://stackoverflow.com/users/69083/guffa):**

> The function is executed right after it's created, not after it is parsed. The entire script block is parsed before any code in it is executed. Also, parsing code doesn't automatically mean that it's executed, if for example the IIFE is inside a function then it won't be executed until the function is called.
