
# How do I check if an array includes an object in JavaScript?

## Question
        
What is the most concise and efficient way to find out if a JavaScript array contains an object?

This is the only way I know to do it:

    function contains(a, obj) {
        for (var i = 0; i < a.length; i++) {
            if (a[i] === obj) {
                return true;
            }
        }
        return false;
    }
    

Is there a better and more concise way to accomplish this?

This is very closely related to Stack Overflow question _[Best way to find an item in a JavaScript Array?](https://stackoverflow.com/questions/143847/best-way-to-find-an-item-in-a-javascript-array)_ which addresses finding objects in an array using `indexOf`.

## Answer
        
Current browsers have [`Array#includes`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/includes#Browser_compatibility), which does _exactly_ that, [is widely supported](https://kangax.github.io/compat-table/es2016plus/#test-Array.prototype.includes), and has a [polyfill](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/includes#Polyfill) for older browsers.

    > ['joe', 'jane', 'mary'].includes('jane');
    true 
    

You can also use [`Array#indexOf`](https://developer.mozilla.org/en/JavaScript/Reference/Global_Objects/Array/indexOf), which is less direct, but doesn't require Polyfills for out of date browsers.

jQuery offers [`$.inArray`](http://api.jquery.com/jquery.inarray/), which is functionally equivalent to `Array#indexOf`.

[underscore.js](http://underscorejs.org/#), a JavaScript utility library, offers [`_.contains(list, value)`](http://underscorejs.org/#contains), alias `_.include(list, value)`, both of which use [indexOf](http://underscorejs.org/#indexOf) internally if passed a JavaScript array.

Some other frameworks offer similar methods:

*   Dojo Toolkit: [`dojo.indexOf(array, value, [fromIndex, findLast])`](http://dojotoolkit.org/reference-guide/dojo/indexOf.html)
*   Prototype: [`array.indexOf(value)`](http://api.prototypejs.org/language/Array/prototype/indexOf/)
*   MooTools: [`array.indexOf(value)`](https://mootools.net/core/docs/1.6.0/Types/Array#Array:indexOf)
*   MochiKit: [`findValue(array, value)`](http://mochi.github.io/mochikit/doc/html/MochiKit/Base.html#fn-findvalue)
*   MS Ajax: [`array.indexOf(value)`](http://www.asp.net/ajaxlibrary/Reference.Array-indexOf-Function.ashx)
*   Ext: [`Ext.Array.contains(array, value)`](http://docs.sencha.com/extjs/4.0.0/#/api/Ext.Array-method-contains)
*   Lodash: [`_.includes(array, value, [from])`](https://lodash.com/docs#includes) (is `_.contains` prior 4.0.0)
*   ECMAScript 2016: [`array.includes(value)`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/includes)

Notice that some frameworks implement this as a function, while others add the function to the array prototype.
