
# Arrow function vs function declaration / expressions: Are they equivalent / exchangeable?

## Question
        
> **Canonical question** If you find a question about issues after replacing a function declaration / expression with an arrow function, please close it as duplicate of this one.

Arrow functions in ES2015 provide a more concise syntax. Can I replace all my function declarations / expressions with arrow functions now? What do I have to look out for?

Examples:

Constructor function

    function User(name) {
      this.name = name;
    }
    
    // vs
    
    const User = name => {
      this.name = name;
    };
    

Prototype methods

    User.prototype.getName = function() {
      return this.name;
    };
    
    // vs
    
    User.prototype.getName = () => this.name;
    

Object (literal) methods

    const obj = {
      getName: function() {
        // ...
      }
    };
    
    // vs
    
    const obj = {
      getName: () => {
        // ...
      }
    };
    

Callbacks

    setTimeout(function() {
      // ...
    }, 500);
    
    // vs
    
    setTimeout(() => {
      // ...
    }, 500);
    

Variadic functions

    function sum() {
      let args = [].slice(arguments);
      // ...
    }
    
    // vs
    const sum = () => {
      let args = [].slice(arguments);
      // ...
    };

## Answer
        
**tl;dr:** **No!** Arrow functions and function declarations / expressions are not equivalent and cannot be replaced blindly.  
If the function you want to replace does _not_ use `this`, `arguments` and is not called with `new`, then yes.

* * *

As so often: **it depends**. Arrow functions have different behavior than function declarations / expressions, so lets have a look at the differences first:

**1\. Lexical `this` and `arguments`**

Arrow functions don't have their own `this` or `arguments` binding. Instead, those identifiers are resolved in the lexical scope like any other variable. That means that inside an arrow function, `this` and `arguments` refer to the values of `this` and `arguments` in the environment the arrow function is _defined_ in (i.e. "outside" the arrow function):

    // Example using a function expression
    function createObject() {
      console.log('Inside `createObject`:', this.foo);
      return {
        foo: 42,
        bar: function() {
          console.log('Inside `bar`:', this.foo);
        },
      };
    }
    
    createObject.call({foo: 21}).bar(); // override `this` inside createObject

    // Example using a arrow function
    function createObject() {
      console.log('Inside `createObject`:', this.foo);
      return {
        foo: 42,
        bar: () => console.log('Inside `bar`:', this.foo),
      };
    }
    
    createObject.call({foo: 21}).bar(); // override `this` inside createObject

In the function expression case, `this` refers to the object that was created inside the `createObject`. In the arrow function case, `this` refers to `this` of `createObject` itself.

This makes arrow functions useful if you need to access the `this` of the current environment:

    // currently common pattern
    var that = this;
    getData(function(data) {
      that.data = data;
    });
    
    // better alternative with arrow functions
    getData(data => {
      this.data = data;
    });
    

**Note** that this also means that is _not_ possible to set an arrow function's `this` with `.bind` or `.call`.

If you are not very familiar with `this`, consider reading

*   [MDN - this](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/this)
*   [YDKJS - this & Object prototypes](https://github.com/getify/You-Dont-Know-JS/blob/master/this%20&%20object%20prototypes/README.md#you-dont-know-js-this--object-prototypes)

**2\. Arrow functions cannot be called with `new`**

ES2015 distinguishes between functions that are _call_able and functions that are _construct_able. If a function is constructable, it can be called with `new`, i.e. `new User()`. If a function is callable, it can be called without `new` (i.e. normal function call).

Functions created through function declarations / expressions are both constructable and callable.  
Arrow functions (and methods) are only callable. `class` constructors are only constructable.

If you are trying to call a non-callable function or to construct a non-constructable function, you will get a runtime error.

* * *

Knowing this, we can state the following.

Replaceable:

*   Functions that don't use `this` or `arguments`.
*   Functions that are used with `.bind(this)`

_Not_ replaceable:

*   Constructor functions
*   Function / methods added to a prototype (because they usually use `this`)
*   Variadic functions (if they use `arguments` (see below))

* * *

Lets have a closer look at this using your examples:

**Constructor function**

This won't work because arrow functions cannot be called with `new`. Keep using a function declaration / expression or use `class`.

**Prototype methods**

Most likely not, because prototype methods usually use `this` to access the instance. If they don't use `this`, then you can replace it. However, if you primarily care for concise syntax, use `class` with its concise method syntax:

    class User {
      constructor(name) {
        this.name = name;
      }
    
      getName() {
        return this.name;
      }
    }
    

**Object methods**

Similarly for methods in an object literal. If the method wants to reference the object itself via `this`, keep using function expressions, or use the new method syntax:

    const obj = {
      getName() {
        // ...
      },
    };
    

**Callbacks**

It depends. You should definitely replace it if you you are aliasing the outer `this` or are using `.bind(this)`:

    // old
    setTimeout(function() {
      // ...
    }.bind(this), 500);
    
    // new
    setTimeout(() => {
      // ...
    }, 500);
    

**But:** If the code which calls the callback explicitly sets `this` to a specific value, as is often the case with event handlers, especially with jQuery, and the callback uses `this` (or `arguments`), you _cannot_ use an arrow function!

**Variadic functions**

Since arrow functions don't have their own `arguments`, you cannot simply replace them with an arrow function. However, ES2015 introduces an alternative to using `arguments`: the [rest parameter](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Functions/rest_parameters).

    // old
    function sum() {
      let args = [].slice.call(arguments);
      // ...
    }
    
    // new
    const sum = (...args) => {
      // ...
    };
    

* * *

Related question:

*   [When should I use Arrow functions in ECMAScript 6?](https://stackoverflow.com/q/22939130/218196)
*   [Do ES6 arrow functions have their own arguments or not?](https://stackoverflow.com/q/33288998/1048572)
*   [What are the differences (if any) between ES6 arrow functions and functions bound with Function.prototype.bind?](https://stackoverflow.com/q/32535110/1048572)
*   [How to use ES6 arrow in class methods?](https://stackoverflow.com/q/31362292/1048572)

Further resources:

*   [MDN - Arrow functions](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Functions/Arrow_functions)
*   [YDKJS - Arrow functions](https://github.com/getify/You-Dont-Know-JS/blob/master/es6%20&%20beyond/ch2.md#arrow-functions)
