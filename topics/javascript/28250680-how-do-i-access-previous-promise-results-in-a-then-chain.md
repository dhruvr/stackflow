
# How do I access previous promise results in a .then() chain?

## Question
        
I have restructured my code to [promises](https://en.wikipedia.org/wiki/Futures_and_promises), and built a wonderful long **flat promise chain**, consisting of multiple `.then()` callbacks. In the end I want to return some composite value, and need to access multiple **intermediate promise results**. However the resolution values from the middle of the sequence are not in scope in the last callback, how do I access them?

    function getExample() {
        return promiseA(…).then(function(resultA) {
            // Some processing
            return promiseB(…);
        }).then(function(resultB) {
            // More processing
            return // How do I gain access to resultA here?
        });
    }

## Answer
        
ECMAScript Harmony
==================

Of course, this problem was recognized by the language designers as well. They did a lot of work and the [async functions proposal](http://tc39.github.io/ecmascript-asyncawait/) finally made it into

ECMAScript 8
------------

You don't need a single `then` invocation or callback function any more, as in an asynchronous function (that returns a promise when being called) you can simply wait for promises to resolve directly. It also features arbitrary control structures like conditions, loops and try-catch-clauses, but for the sake of convenience we don't need them here:

    async function getExample() {
        var resultA = await promiseA(…);
        // some processing
        var resultB = await promiseB(…);
        // more processing
        return // something using both resultA and resultB
    }
    

ECMAScript 6
------------

While we were waiting for ES8, we already did use a very similar kind of syntax. ES6 came with [generator functions](http://davidwalsh.name/es6-generators), which allow to break the execution apart in pieces at arbitrarily placed `yield` keywords. Those slices can be run after each other, independently, even asynchronously - and that's just what we do when we want to wait for a promise resolution before running the next step.

There are dedicated libraries (like [co](https://github.com/tj/co) or [task.js](http://taskjs.org/)), but also many promise libraries have helper functions ([Q](https://github.com/kriskowal/q/wiki/API-Reference#generators), [Bluebird](http://bluebirdjs.com/docs/api/generators.html), [when](https://github.com/cujojs/when/blob/master/docs/api.md#es6-generators), …) that do [this async step-by-step execution](https://stackoverflow.com/a/23554399/1048572) for you when you give them a generator function that yields promises.

    var getExample = Promise.coroutine(function* () {
    //               ^^^^^^^^^^^^^^^^^ Bluebird syntax
        var resultA = yield promiseA(…);
        // some processing
        var resultB = yield promiseB(…);
        // more processing
        return // something using both resultA and resultB
    });
    

This did work in Node.js since version 4.0, also a few browsers (or their dev editions) did support generator syntax relatively early.

ECMAScript 5
------------

However, if you want/need to be backwards-compatible you cannot use those without a transpiler. Both generator functions and async functions are supported by the current tooling, see for example the documentation of Babel on [generators](http://babeljs.io/learn-es2015/#ecmascript-2015-features-generators) and [async functions](http://babeljs.io/docs/plugins/syntax-async-functions).

And then, there are also many other [compile-to-JS languages](https://github.com/jashkenas/coffeescript/wiki/List-of-languages-that-compile-to-JS#synchronous-to-asynchronous-javascript-compilers-cps) that are dedicated to easing asynchronous programming. They usually use a syntax similar to `await`, (e.g. [Iced CoffeeScript](http://maxtaco.github.io/coffee-script/)), but there are also others that feature a Haskell-like `do`-notation (e.g. [LatteJs](http://lattejs.com/), [monadic](https://www.npmjs.com/package/monadic), [PureScript](http://www.purescript.org/) or [LispyScript](http://lispyscript.com/)).
