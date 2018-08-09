
# What is the !! (not not) operator in JavaScript?

## Question
        
I saw some code that seems to use an operator I don't recognize, in the form of two exclamation points, like so: `!!`. Can someone please tell me what this operator does?

The context in which I saw this was,

    this.vertical = vertical !== undefined ? !!vertical : this.vertical;

## Answer
        
Coerces `oObject` to boolean. If it was falsey (e.g. 0, `null`, `undefined`, etc.), it will be `false`, otherwise, `true`.

    !oObject  //Inverted boolean
    !!oObject //Non inverted boolean so true boolean representation
    

So `!!` is not an operator, it's just the `!` operator twice.

Real World Example "Test IE version":

    let isIE8 = false;  
    isIE8 = !! navigator.userAgent.match(/MSIE 8.0/);  
    console.log(isIE8); // returns true or false 
    

If you ⇒

    console.log(navigator.userAgent.match(/MSIE 8.0/));  
    // returns null  
    

but if you ⇒

    console.log(!!navigator.userAgent.match(/MSIE 8.0/));  
    // returns true or false
