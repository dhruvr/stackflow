
# What&apos;s the meaning of &#x201C;=&gt;&#x201D; (an arrow formed from equals &amp; greater than) in JavaScript?

## Question
        
I know that the `>=` operator means more than or equal to, but I've seen `=>` in some source code. What's the meaning of that operator?

Here's the code:

    promiseTargetFile(fpParams, aSkipPrompt, relatedURI).then(aDialogAccepted => {
          if (!aDialogAccepted)
            return;
    
          saveAsType = fpParams.saveAsType;
          file = fpParams.file;
    
          continueSave();
        }).then(null, Components.utils.reportError);
      }

## Answer
        
What It Is
==========

**This is an arrow function.** Arrow functions are a short syntax, introduced by ECMAscript 6, that can be used similarly to the way you would use function expressions. In other words, you can often use them in place of expressions like `function (foo) {...}`. But they have some important differences. For example, they do not bind their own values of `this` (see below for discussion).

Arrow functions are part of the ECMAscript 6 specification, but not part of "normal" JavaScript in use in most browsers today. They are, however, [partially supported in Node v. 4.0+](https://kangax.github.io/compat-table/es6/) and in many browsers (see below).

[You can read more in **the Mozilla documentation** on arrow functions](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/arrow_functions).

From the Mozilla documentation:

> An arrow function expression (also known as fat arrow function) has a shorter syntax compared to [function expressions](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/function) and lexically binds the [`this`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/this) value (does not bind its own [`this`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/this), [`arguments`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Functions/arguments), [`super`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/super), or [`new.target`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/new.target)). Arrow functions are always anonymous. These function expressions are best suited for non-method functions and they can not be used as constructors.

A Note on How `this` Works in Arrow Functions
---------------------------------------------

One of the most handy features of an arrow function is buried in the text above:

> An arrow function... lexically binds the `this` value (does not bind its own `this`...)

What this means in simpler terms is that the arrow function retains the `this` value from its context and does not have its own `this`. A traditional function _does_ bind its own `this` value, requiring lots of gymnastics like `self = this;`, etc., to access or manipulate `this` from one function inside another function. For more info on this topic, see [the explanation and examples in the Mozilla documentation](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Functions/Arrow_functions#Lexical_this).

Example Code
============

Example (also from the docs):

    var a = [
      "We're up all night 'til the sun",
      "We're up all night to get some",
      "We're up all night for good fun",
      "We're up all night to get lucky"
    ];
    
    // These two assignments are equivalent:
    
    // Old-school:
    var a2 = a.map(function(s){ return s.length });
    
    // ECMAscript 6 using arrow functions
    var a3 = a.map( s => s.length );
    
    // both a2 and a3 will be equal to [31, 30, 31, 31]
    

* * *

Notes on Compatibility
======================

You can use arrow functions in Node, but browser support is spotty.

Browser support for this functionality has improved quite a bit, but it still is not widespread enough for most browser-based usages. As of December 12, 2017, it is supported in current versions of:

*   Chrome (v. 45+)
*   Firefox (v. 22+)
*   Edge (v. 12+)
*   Opera (v. 32+)
*   Android Browser (v. 47+)
*   Opera Mobile (v. 33+)
*   Chrome for Android (v. 47+)
*   Firefox for Android (v. 44+)
*   Safari (v. 10+)
*   iOS Safari (v. 10.2+)
*   Samsung Internet (v. 5+)
*   Baidu Browser (v. 7.12+)

Not supported in:

*   IE (through v. 11)
*   Opera Mini (through v. 8.0)
*   Blackberry Browser (through v. 10)
*   IE Mobile (through v. 11)
*   UC Browser for Android (through v. 11.4)
*   QQ (through v. 1.2)

You can find more (and more current) information at [CanIUse.com](http://caniuse.com/arrow-functions) (no affiliation).
