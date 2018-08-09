
# How do I return the response from an asynchronous call?

## Question
        
I have a function `foo` which makes an Ajax request. How can I return the response from `foo`?

I tried returning the value from the `success` callback as well as assigning the response to a local variable inside the function and returning that one, but none of those ways actually return the response.

    function foo() {
        var result;
    
        $.ajax({
            url: '...',
            success: function(response) {
                result = response;
                // return response; // <- I tried that one as well
            }
        });
    
        return result;
    }
    
    var result = foo(); // It always ends up being `undefined`.

## Answer
        
> _`->` For a more general explanation of async behaviour with different examples, please see_ [Why is my variable unaltered after I modify it inside of a function? - Asynchronous code reference](https://stackoverflow.com/q/23667086/218196)
> 
> _`->` If you already understand the problem, skip to the possible solutions below._

The problem
===========

The **A** in [Ajax](https://en.wikipedia.org/wiki/Ajax_(programming)) stands for [**asynchronous**](https://www.merriam-webster.com/dictionary/asynchronous) . That means sending the request (or rather receiving the response) is taken out of the normal execution flow. In your example, `$.ajax` returns immediately and the next statement, `return result;`, is executed before the function you passed as `success` callback was even called.

Here is an analogy which hopefully makes the difference between synchronous and asynchronous flow clearer:

Synchronous
-----------

Imagine you make a phone call to a friend and ask him to look something up for you. Although it might take a while, you wait on the phone and stare into space, until your friend gives you the answer that you needed.

The same is happening when you make a function call containing "normal" code:

    function findItem() {
        var item;
        while(item_not_found) {
            // search
        }
        return item;
    }
    
    var item = findItem();
    
    // Do something with item
    doSomethingElse();
    

Even though `findItem` might take a long time to execute, any code coming after `var item = findItem();` has to _wait_ until the function returns the result.

Asynchronous
------------

You call your friend again for the same reason. But this time you tell him that you are in a hurry and he should _call you back_ on your mobile phone. You hang up, leave the house and do whatever you planned to do. Once your friend calls you back, you are dealing with the information he gave to you.

That's exactly what's happening when you do an Ajax request.

    findItem(function(item) {
        // Do something with item
    });
    doSomethingElse();
    

Instead of waiting for the response, the execution continues immediately and the statement after the Ajax call is executed. To get the response eventually, you provide a function to be called once the response was received, a _callback_ (notice something? _call back_ ?). Any statement coming after that call is executed before the callback is called.

* * *

Solution(s)
===========

**Embrace the asynchronous nature of JavaScript!** While certain asynchronous operations provide synchronous counterparts (so does "Ajax"), it's generally discouraged to use them, especially in a browser context.

Why is it bad do you ask?

JavaScript runs in the UI thread of the browser and any long running process will lock the UI, making it unresponsive. Additionally, there is an upper limit on the execution time for JavaScript and the browser will ask the user whether to continue the execution or not.

All of this is really bad user experience. The user won't be able to tell whether everything is working fine or not. Furthermore, the effect will be worse for users with a slow connection.

In the following we will look at three different solutions that are all building on top of each other:

*   **Promises with `async/await`** (ES2017+, available in older browsers if you use a transpiler or regenerator)
*   **Callbacks** (popular in node)
*   **Promises with `then()`** (ES2015+, available in older browsers if you use one of the many promise libraries)

**All three are available in current browsers, and node 7+.**

* * *

ES2017+: Promises with [`async/await`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/async_function)
-----------------------------------------------------------------------------------------------------------------------------------

The new ECMAScript version released in 2017 introduced _syntax-level support_ for asynchronous functions. With the help of `async` and `await`, you can write asynchronous in a "synchronous style". Make no mistake though: The code is still asynchronous, but it's easier to read/understand.

`async/await` builds on top of promises: an `async` function always returns a promise. `await` "unwraps" a promise and either result in the value the promise was resolved with or throws an error if the promise was rejected.

**Important:** You can only use `await` inside an `async` function. That means that at the very top level, you still have to work directly with the promise.

You can read more about [`async`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/async_function) and [`await`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/await) on MDN.

Here is an example that builds on top of delay above:

    // Using 'superagent' which will return a promise.
    var superagent = require('superagent')
    
    // This is isn't declared as `async` because it already returns a promise
    function delay() {
      // `delay` returns a promise
      return new Promise(function(resolve, reject) {
        // Only `delay` is able to resolve or reject the promise
        setTimeout(function() {
          resolve(42); // After 3 seconds, resolve the promise with value 42
        }, 3000);
      });
    }
    
    
    async function getAllBooks() {
      try {
        // GET a list of book IDs of the current user
        var bookIDs = await superagent.get('/user/books');
        // wait for a second (just for the sake of this example)
        await delay(1000);
        // GET information about each book
        return await superagent.get('/books/ids='+JSON.stringify(bookIDs));
      } catch(error) {
        // If any of the awaited promises was rejected, this catch block
        // would catch the rejection reason
        return null;
      }
    }
    
    // Async functions always return a promise
    getAllBooks()
      .then(function(books) {
        console.log(books);
      });
    

Newer [browser](https://kangax.github.io/compat-table/es2016plus/#test-async_functions) and [node](http://node.green/#ES2017-features-async-functions) versions support `async/await`. You can also support older environments by transforming your code to ES5 with the help of [regenerator](https://github.com/facebook/regenerator) (or tools that use regenerator, such as [Babel](https://babeljs.io/)).

* * *

Let functions accept _callbacks_
--------------------------------

A callback is simply a function passed to another function. That other function can call the function passed whenever it is ready. In the context of an asynchronous process, the callback will be called whenever the asynchronous process is done. Usually, the result is passed to the callback.

In the example of the question, you can make `foo` accept a callback and use it as `success` callback. So this

    var result = foo();
    // Code that depends on 'result'
    

becomes

    foo(function(result) {
        // Code that depends on 'result'
    });
    

Here we defined the function "inline" but you can pass any function reference:

    function myCallback(result) {
        // Code that depends on 'result'
    }
    
    foo(myCallback);
    

`foo` itself is defined as follows:

    function foo(callback) {
        $.ajax({
            // ...
            success: callback
        });
    }
    

`callback` will refer to the function we pass to `foo` when we call it and we simply pass it on to `success`. I.e. once the Ajax request is successful, `$.ajax` will call `callback` and pass the response to the callback (which can be referred to with `result`, since this is how we defined the callback).

You can also process the response before passing it to the callback:

    function foo(callback) {
        $.ajax({
            // ...
            success: function(response) {
                // For example, filter the response
                callback(filtered_response);
            }
        });
    }
    

It's easier to write code using callbacks than it may seem. After all, JavaScript in the browser is heavily event-driven (DOM events). Receiving the Ajax response is nothing else but an event.  
Difficulties could arise when you have to work with third-party code, but most problems can be solved by just thinking through the application flow.

* * *

ES2015+: Promises with [then()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Promise)
-------------------------------------------------------------------------------------------------------------------------

The [Promise API](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Promise) is a new feature of ECMAScript 6 (ES2015), but it has good [browser support](http://caniuse.com/#feat=promises "caniuse") already. There are also many libraries which implement the standard Promises API and provide additional methods to ease the use and composition of asynchronous functions (e.g. [bluebird](https://github.com/petkaantonov/bluebird)).

Promises are containers for _future_ values. When the promise receives the value (it is _resolved_) or when it is cancelled (_rejected_), it notifies all of its "listeners" who want to access this value.

The advantage over plain callbacks is that they allow you to decouple your code and they are easier to compose.

Here is a simple example of using a promise:

    function delay() {
      // `delay` returns a promise
      return new Promise(function(resolve, reject) {
        // Only `delay` is able to resolve or reject the promise
        setTimeout(function() {
          resolve(42); // After 3 seconds, resolve the promise with value 42
        }, 3000);
      });
    }
    
    delay()
      .then(function(v) { // `delay` returns a promise
        console.log(v); // Log the value once it is resolved
      })
      .catch(function(v) {
        // Or do something else if it is rejected 
        // (it would not happen in this example, since `reject` is not called).
      });
    

Applied to our Ajax call we could use promises like this:

    function ajax(url) {
      return new Promise(function(resolve, reject) {
        var xhr = new XMLHttpRequest();
        xhr.onload = function() {
          resolve(this.responseText);
        };
        xhr.onerror = reject;
        xhr.open('GET', url);
        xhr.send();
      });
    }
    
    ajax("/echo/json")
      .then(function(result) {
        // Code depending on result
      })
      .catch(function() {
        // An error occurred
      });
    

Describing all the advantages that promise offer is beyond the scope of this answer, but if you write new code, you should seriously consider them. They provide a great abstraction and separation of your code.

More information about promises: [HTML5 rocks - JavaScript Promises](http://www.html5rocks.com/en/tutorials/es6/promises/)

### Side note: jQuery's deferred objects

[Deferred objects](https://stackoverflow.com/questions/4866721/what-are-deferred-objects) are jQuery's custom implementation of promises (before the Promise API was standardized). They behave almost like promises but expose a slightly different API.

Every Ajax method of jQuery already returns a "deferred object" (actually a promise of a deferred object) which you can just return from your function:

    function ajax() {
        return $.ajax(...);
    }
    
    ajax().done(function(result) {
        // Code depending on result
    }).fail(function() {
        // An error occurred
    });
    

### Side note: Promise gotchas

Keep in mind that promises and deferred objects are just _containers_ for a future value, they are not the value itself. For example, suppose you had the following:

    function checkPassword() {
        return $.ajax({
            url: '/password',
            data: {
                username: $('#username').val(),
                password: $('#password').val()
            },
            type: 'POST',
            dataType: 'json'
        });
    }
    
    if (checkPassword()) {
        // Tell the user they're logged in
    }
    

This code misunderstands the above asynchrony issues. Specifically, `$.ajax()` doesn't freeze the code while it checks the '/password' page on your server - it sends a request to the server and while it waits, immediately returns a jQuery Ajax Deferred object, not the response from the server. That means the `if` statement is going to always get this Deferred object, treat it as `true`, and proceed as though the user is logged in. Not good.

But the fix is easy:

    checkPassword()
    .done(function(r) {
        if (r) {
            // Tell the user they're logged in
        } else {
            // Tell the user their password was bad
        }
    })
    .fail(function(x) {
        // Tell the user something bad happened
    });
    

* * *

Not recommended: Synchronous "Ajax" calls
-----------------------------------------

As I mentioned, some(!) asynchronous operations have synchronous counterparts. I don't advocate their use, but for completeness' sake, here is how you would perform a synchronous call:

### Without jQuery

If you directly use a [`XMLHTTPRequest`](http://www.w3.org/TR/XMLHttpRequest/) object, pass `false` as third argument to [`.open`](http://www.w3.org/TR/XMLHttpRequest/#the-open%28%29-method).

### jQuery

If you use [jQuery](http://api.jquery.com/jQuery.ajax/), you can set the `async` option to `false`. Note that this option is _deprecated_ since jQuery 1.8. You can then either still use a `success` callback or access the `responseText` property of the [jqXHR object](http://api.jquery.com/jQuery.ajax/#jqXHR):

    function foo() {
        var jqXHR = $.ajax({
            //...
            async: false
        });
        return jqXHR.responseText;
    }
    

If you use any other jQuery Ajax method, such as `$.get`, `$.getJSON`, etc., you have to change it to `$.ajax` (since you can only pass configuration parameters to `$.ajax`).

**Heads up!** It is not possible to make a synchronous [JSONP](https://stackoverflow.com/questions/2067472/please-explain-jsonp) request. JSONP by its very nature is always asynchronous (one more reason to not even consider this option).
