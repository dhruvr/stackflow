
# What is the JavaScript version of sleep()?

## Question
        
Is there a better way to engineer a `sleep` in JavaScript than the following `pausecomp` function ([taken from here](http://www.sean.co.uk/a/webdesign/javascriptdelay.shtm))?

    function pausecomp(millis)
    {
        var date = new Date();
        var curDate = null;
        do { curDate = new Date(); }
        while(curDate-date < millis);
    }
    

This is not a duplicate of [Sleep in JavaScript - delay between actions](https://stackoverflow.com/questions/758688/sleep-in-javascript-delay-between-actions); I want a _real sleep_ in the middle of a function, and not a delay before a piece of code executes.

## Answer
        
2017 update
-----------

Since 2009 when this question was asked, JavaScript has evolved significantly. All other answers are now obsolete or overly complicated. Here is the current best practice:

    function sleep(ms) {
      return new Promise(resolve => setTimeout(resolve, ms));
    }
    
    async function demo() {
      console.log('Taking a break...');
      await sleep(2000);
      console.log('Two second later');
    }
    
    demo();

### This is it. `await sleep(<duration>)`.

You can try this code live [on Runkit](https://runkit.com/dandv/57f770a7aed68d0014e7b660). Note that,

1.  `await` can only be executed in functions prefixed with the `async` keyword. Runkit wraps your code in an async function before executing it.
2.  `await` only pauses the current `async` function

Two new JavaScript features helped write this actual "sleep" function:

*   [Promises, a native feature of ES2015](https://ponyfoo.com/articles/es6-promises-in-depth) (aka ES6). We also use [arrow functions](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Functions/Arrow_functions) in the definition of the sleep function.
*   The [upcoming](https://github.com/tc39/ecmascript-asyncawait) [`async/await`](https://ponyfoo.com/articles/understanding-javascript-async-await) feature lets the code explicitly wait for a promise to settle.

Compatibility
-------------

*   promises are supported [in Node v0.12+](http://node.green/#Promise) and [widely supported in browsers](http://caniuse.com/#feat=promises), except IE
*   `async`/`await` landed in V8 and has been [enabled by default since Chrome 55](https://developers.google.com/web/fundamentals/getting-started/primers/async-functions)
    *   it landed [in Node 7 in October 2016](https://blog.risingstack.com/async-await-node-js-7-nightly/)
    *   has also landed [in Firefox Nightly in November 2016](https://blog.nightly.mozilla.org/2016/11/01/async-await-support-in-firefox/)

If for some reason you're using Node older than 7, or are targeting old browsers, `async`/`await` can still be used via [Babel](https://babeljs.io/) (a tool that will [transpile](https://www.stevefenton.co.uk/2012/11/compiling-vs-transpiling/) JavaScript + new features into plain old JavaScript), with the [`transform-async-to-generator` plugin](https://babeljs.io/docs/plugins/transform-async-to-generator/). Run

    npm install babel-cli --save
    

Create `.babelrc` with:

    {
      "plugins": [
        "transform-async-to-generator",
      ]
    }
    

Then run your code with

    node_modules/babel-cli/bin/babel-node.js sleep.js
    

But again, you don't need this if you're using Node 7 or later, or if you're targeting modern browsers.
