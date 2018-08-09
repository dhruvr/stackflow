
# JavaScript property access: dot notation vs. brackets?

## Question
        
Other than the obvious fact that the first form could use a variable and not just a string literal, is there any reason to use one over the other, and if so under which cases?

In code:

    // Given:
    var foo = {'bar': 'baz'};
    
    // Then
    var x = foo['bar'];
    
    // vs. 
    var x = foo.bar;
    

Context: I've written a code generator which produces these expressions and I'm wondering which is preferable.

## Answer
        
(Sourced from [here](http://www.dev-archive.net/articles/js-dot-notation/).)

**Square bracket notation allows the use of characters that can't be used with dot notation:**

>     var foo = myForm.foo[]; // incorrect syntax
>     var foo = myForm["foo[]"]; // correct syntax
>     

**Secondly, square bracket notation is useful when dealing with property names which vary in a predictable way:**

>     for (var i = 0; i < 10; i++) {
>       someFunction(myForm["myControlNumber" + i]);
>     }
>     

**Roundup:**

> *   Dot notation is faster to write and clearer to read.
> *   Square bracket notation allows access to properties containing special characters and selection of properties using variables

* * *

Another example of characters that can't be used with dot notation is _property names that themselves contain a dot_.

For example a json response could contain a property called `bar.Baz`.

    var foo = myResponse.bar.Baz; // incorrect syntax
    var foo = myResponse["bar.Baz"]; // correct syntax
