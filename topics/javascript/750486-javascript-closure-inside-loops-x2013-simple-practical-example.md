
# JavaScript closure inside loops &#x2013; simple practical example

## Question
        
    var funcs = [];
    for (var i = 0; i < 3; i++) {      // let's create 3 functions
      funcs[i] = function() {          // and store them in funcs
        console.log("My value: " + i); // each should log its value.
      };
    }
    for (var j = 0; j < 3; j++) {
      funcs[j]();                      // and now let's run each one to see
    }

It outputs this:

> My value: 3  
> My value: 3  
> My value: 3

Whereas I'd like it to output:

> My value: 0  
> My value: 1  
> My value: 2

* * *

The same problem occurs when the delay in running the function is caused by using event listeners:

    var buttons = document.getElementsByTagName("button");
    for (var i = 0; i < buttons.length; i++) {          // let's create 3 functions
      buttons[i].addEventListener("click", function() { // as event listeners
        console.log("My value: " + i);                  // each should log its value.
      });
    }

    <button>0</button><br>
    <button>1</button><br>
    <button>2</button>

… or asynchronous code, e.g. using Promises:

    // Some async wait function
    const wait = (ms) => new Promise((resolve, reject) => setTimeout(resolve, ms));
    
    for(var i = 0; i < 3; i++){
      wait(i * 100).then(() => console.log(i)); // Log `i` as soon as each promise resolves.
    }

What’s the solution to this basic problem?

## Answer
        
Well, the problem is that the variable `i`, within each of your anonymous functions, is bound to the same variable outside of the function.

Classic solution: Closures
==========================

What you want to do is bind the variable within each function to a separate, unchanging value outside of the function:

    var funcs = [];
    
    function createfunc(i) {
        return function() { console.log("My value: " + i); };
    }
    
    for (var i = 0; i < 3; i++) {
        funcs[i] = createfunc(i);
    }
    
    for (var j = 0; j < 3; j++) {
        funcs[j]();                        // and now let's run each one to see
    }

Since there is no block scope in JavaScript - only function scope - by wrapping the function creation in a new function, you ensure that the value of "i" remains as you intended.

* * *

2015 Solution: forEach
======================

With the relatively widespread availability of the `Array.prototype.forEach` function (in 2015), it's worth noting that in those situations involving iteration primarily over an array of values, `.forEach()` provides a clean, natural way to get a distinct closure for every iteration. That is, assuming you've got some sort of array containing values (DOM references, objects, whatever), and the problem arises of setting up callbacks specific to each element, you can do this:

    var someArray = [ /* whatever */ ];
    // ...
    someArray.forEach(function(arrayElement) {
      // ... code code code for this one element
      someAsynchronousFunction(arrayElement, function() {
        arrayElement.doSomething();
      });
    });
    

The idea is that each invocation of the callback function used with the `.forEach` loop will be its own closure. The parameter passed in to that handler is the array element specific to that particular step of the iteration. If it's used in an asynchronous callback, it won't collide with any of the other callbacks established at other steps of the iteration.

If you happen to be working in jQuery, the `$.each()` function gives you a similar capability.

* * *

ES6 solution: `let`
===================

ECMAScript 6 (ES6), the newest version of JavaScript, is now starting to be implemented in many evergreen browsers and backend systems. There are also transpilers like [Babel](http://babeljs.io/) that will convert ES6 to ES5 to allow usage of new features on older systems.

ES6 introduces new `let` and `const` keywords that are scoped differently than `var`-based variables. For example, in a loop with a `let`-based index, each iteration through the loop will have a new value of `i` where each value is scoped inside the loop, so your code would work as you expect. There are many resources, but I'd recommend [2ality's block-scoping post](http://www.2ality.com/2015/02/es6-scoping.html) as a great source of information.

    for (let i = 0; i < 3; i++) {
        funcs[i] = function() {
            console.log("My value: " + i);
        };
    }
    

Beware, though, that IE9-IE11 and Edge prior to Edge 14 support `let` but get the above wrong (they don't create a new `i` each time, so all the functions above would log 3 like they would if we used `var`). Edge 14 finally gets it right.
