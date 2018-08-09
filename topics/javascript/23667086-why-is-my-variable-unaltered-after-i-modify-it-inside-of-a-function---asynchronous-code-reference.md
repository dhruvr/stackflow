
# Why is my variable unaltered after I modify it inside of a function? - Asynchronous code reference

## Question
        
Given the following examples, why is `outerScopeVar` undefined in all cases?

    var outerScopeVar;
    
    var img = document.createElement('img');
    img.onload = function() {
        outerScopeVar = this.width;
    };
    img.src = 'lolcat.png';
    alert(outerScopeVar);
    

    var outerScopeVar;
    setTimeout(function() {
        outerScopeVar = 'Hello Asynchronous World!';
    }, 0);
    alert(outerScopeVar);
    

    // Example using some jQuery
    var outerScopeVar;
    $.post('loldog', function(response) {
        outerScopeVar = response;
    });
    alert(outerScopeVar);
    

    // Node.js example
    var outerScopeVar;
    fs.readFile('./catdog.html', function(err, data) {
        outerScopeVar = data;
    });
    console.log(outerScopeVar);
    

    // with promises
    var outerScopeVar;
    myPromise.then(function (response) {
        outerScopeVar = response;
    });
    console.log(outerScopeVar);
    

    // geolocation API
    var outerScopeVar;
    navigator.geolocation.getCurrentPosition(function (pos) {
        outerScopeVar = pos;
    });
    console.log(outerScopeVar);
    

Why does it output `undefined` in all of these examples? I don't want workarounds, I want to know **why** this is happening.

* * *

> **Note:** This is a canonical question for _JavaScript asynchronicity_. Feel free to improve this question and add more simplified examples which the community can identify with.

## Answer
        
One word answer: **asynchronicity**.

Forewords
---------

This topic has been iterated at least a couple of thousands of times, here, in Stack Overflow. Hence, first off I'd like to point out some extremely useful resources:

*   [@Felix Kling's "How to return the response from an AJAX call"](https://stackoverflow.com/a/14220323/1331430). See his excellent answer explaining synchronous and asynchronous flows, as well as the "Restructure code" section.  
    @Benjamin Gruenbaum has also put a lot of effort explaining asynchronicity in the same thread.
    
*   [@Matt Esch's answer to "Get data from fs.readFile"](https://stackoverflow.com/a/10058879/1331430) also explains asynchronicity extremely well in a simple manner.
    

* * *

The answer to the question at hand
----------------------------------

Let's trace the common behavior first. In all examples, the `outerScopeVar` is modified inside of a _function_. That function is clearly not executed immediately, it is being assigned or passed as an argument. That is what we call a **_callback_**.

Now the question is, when is that callback called?

It depends on the case. Let's try to trace some common behavior again:

*   `img.onload` may be called _sometime in the future_, when (and if) the image has successfully loaded.
*   `setTimeout` may be called _sometime in the future_, after the delay has expired and the timeout hasn't been canceled by `clearTimeout`. Note: even when using `0` as delay, all browsers have a minimum timeout delay cap (specified to be 4ms in the HTML5 spec).
*   jQuery `$.post`'s callback may be called _sometime in the future_, when (and if) the Ajax request has been completed successfully.
*   Node.js's `fs.readFile` may be called _sometime in the future_, when the file has been read successfully or thrown an error.

In all cases, we have a callback which may run _sometime in the future_. This "sometime in the future" is what we refer to as **asynchronous flow**.

Asynchronous execution is pushed out of the synchronous flow. That is, the asynchronous code will **never** execute while the synchronous code stack is executing. This is the meaning of JavaScript being single-threaded.

More specifically, when the JS engine is idle -- not executing a stack of (a)synchronous code -- it will poll for events that may have triggered asynchronous callbacks (e.g. expired timeout, received network response) and execute them one after another. This is regarded as [Event Loop](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/EventLoop).

That is, the asynchronous code highlighted in the hand-drawn red shapes may execute only after all the remaining synchronous code in their respective code blocks have executed:

![async code highlighted](https://i.stack.imgur.com/40IwM.png)

In short, the callback functions are created synchronously but executed asynchronously. You just can't rely on the execution of an asynchronous function until you know it has executed, and how to do that?

It is simple, really. The logic that depends on the asynchronous function execution should be started/called from inside this asynchronous function. For example, moving the `alert`s and `console.log`s too inside the callback function would output the expected result, because the result is available at that point.

### Implementing your own callback logic

Often you need to do more things with the result from an asynchronous function or do different things with the result depending on where the asynchronous function has been called. Let's tackle a bit more complex example:

    var outerScopeVar;
    helloCatAsync();
    alert(outerScopeVar);
    
    function helloCatAsync() {
        setTimeout(function() {
            outerScopeVar = 'Nya';
        }, Math.random() * 2000);
    }
    

**Note:** I'm using `setTimeout` with a random delay as a generic asynchronous function, the same example applies to Ajax, `readFile`, `onload` and any other asynchronous flow.

This example clearly suffers from the same issue as the other examples, it is not waiting until the asynchronous function executes.

Let's tackle it implementing a callback system of our own. First off, we get rid of that ugly `outerScopeVar` which is completely useless in this case. Then we add a parameter which accepts a function argument, our callback. When the asynchronous operation finishes, we call this callback passing the result. The implementation (please read the comments in order):

    // 1. Call helloCatAsync passing a callback function,
    //    which will be called receiving the result from the async operation
    helloCatAsync(function(result) {
        // 5. Received the result from the async function,
        //    now do whatever you want with it:
        alert(result);
    });
    
    // 2. The "callback" parameter is a reference to the function which
    //    was passed as argument from the helloCatAsync call
    function helloCatAsync(callback) {
        // 3. Start async operation:
        setTimeout(function() {
            // 4. Finished async operation,
            //    call the callback passing the result as argument
            callback('Nya');
        }, Math.random() * 2000);
    }
    

Most often in real use cases, the DOM API and most libraries already provide the callback functionality (the `helloCatAsync` implementation in this demonstrative example). You only need to pass the callback function and understand that it will execute out of the synchronous flow, and restructure your code to accommodate for that.

You will also notice that due to the asynchronous nature, it is impossible to `return` a value from an asynchronous flow back to the synchronous flow where the callback was defined, as the asynchronous callbacks are executed long after the synchronous code has already finished executing.

Instead of `return`ing a value from an asynchronous callback, you will have to make use of the callback pattern, or... Promises.

### Promises

Although there are ways to keep the [callback hell](http://callbackhell.com/) at bay with vanilla JS, promises are growing in popularity and are currently being standardized in ES6 (see [Promise - MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Promise)).

Promises (a.k.a. Futures) provide a more linear, and thus pleasant, reading of the asynchronous code, but explaining their entire functionality is out of the scope of this question. Instead, I'll leave these excellent resources for the interested:

*   [JavaScript Promises - HTML5 Rocks](http://www.html5rocks.com/en/tutorials/es6/promises/)
*   [You're Missing the Point of Promises - domenic.me](http://domenic.me/2012/10/14/youre-missing-the-point-of-promises/)

* * *

### More reading material about JavaScript asynchronicity

*   [The Art of Node - Callbacks](https://github.com/maxogden/art-of-node#callbacks) explains asynchronous code and callbacks very well with vanilla JS examples and Node.js code as well.

* * *

> **Note:** I've marked this answer as Community Wiki, hence anyone with at least 100 reputations can edit and improve it! Please feel free to improve this answer, or submit a completely new answer if you'd like as well.
> 
> I want to turn this question into a canonical topic to answer asynchronicity issues which are unrelated to Ajax (there is [How to return the response from an AJAX call?](https://stackoverflow.com/q/14220321/1331430) for that), hence this topic needs your help to be as good and helpful as possible!
