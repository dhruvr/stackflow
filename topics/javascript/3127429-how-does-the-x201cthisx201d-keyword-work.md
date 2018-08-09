
# How does the &#x201C;this&#x201D; keyword work?

## Question
        
I have noticed that there doesn't appear to be a clear explanation of what the `this` keyword is and how it is correctly (and incorrectly) used in JavaScript on the Stack Overflow site.

I have witnessed some very strange behaviour with it and have failed to understand why it has occurred.

How does `this` work and when should it be used?

## Answer
        
I recommend reading [Mike West](https://mikewest.org)'s article [Scope in JavaScript](http://www.digital-web.com/articles/scope_in_javascript/) ([mirror](http://web.archive.org/web/20110725013125/http://www.digital-web.com/articles/scope_in_javascript/)) first. It is an excellent, friendly introduction to the concepts of `this` and scope chains in JavaScript.

Once you start getting used to `this`, the rules are actually pretty simple. The [ECMAScript 5.1 Standard](https://ecma-international.org/ecma-262/5.1) defines `this`:

> ### [§11.1.1](https://ecma-international.org/ecma-262/5.1/#sec-11.1.1) The `this` keyword
> 
> The `this` keyword evaluates to the value of the ThisBinding of the current execution context

ThisBinding is something that the JavaScript interpreter maintains as it evaluates JavaScript code, like a special CPU register which holds a reference to an object. The interpreter updates the ThisBinding whenever establishing an execution context in one of only three different cases:

### 1\. Initial global execution context

This is the case for JavaScript code that is evaluated at the top-level, e.g. when directly inside a `<script>`:

    <script>
      alert("I'm evaluated in the initial global execution context!");
    
      setTimeout(function () {
          alert("I'm NOT evaluated in the initial global execution context.");
      }, 1);
    </script>
    

When evaluating code in the initial global execution context, ThisBinding is set to the global object, `window` ([§10.4.1.1](https://ecma-international.org/ecma-262/5.1/#sec-10.4.1.1)).

### Entering eval code

*   …by a direct call to `eval()` ThisBinding is left unchanged; it is the same value as the ThisBinding of the calling execution context ([§10.4.2](https://ecma-international.org/ecma-262/5.1/#sec-10.4.2) (2)(a)).
    
*   …if not by a direct call to `eval()`  
    ThisBinding is set to the global object _as if_ executing in the initial global execution context ([§10.4.2](https://ecma-international.org/ecma-262/5.1/#sec-10.4.2) (1)).
    

§15.1.2.1.1 defines what a direct call to `eval()` is. Basically, `eval(...)` is a direct call whereas something like `(0, eval)(...)` or `var indirectEval = eval; indirectEval(...);` is an indirect call to `eval()`. See [chuckj's answer](https://stackoverflow.com/a/9107491/196844) to [(1, eval)('this') vs eval('this') in JavaScript?](https://stackoverflow.com/q/9107240/196844) and [Dmitry Soshnikov’s ECMA-262-5 in detail. Chapter 2. Strict Mode.](http://dmitrysoshnikov.com/ecmascript/es5-chapter-2-strict-mode/#indirect-eval-call) for when you might use an indirect `eval()` call.

### Entering function code

This occurs when calling a function. If a function is called on an object, such as in `obj.myMethod()` or the equivalent `obj["myMethod"]()`, then ThisBinding is set to the object (`obj` in the example; [§13.2.1](https://ecma-international.org/ecma-262/5.1/#sec-13.2.1)). In most other cases, ThisBinding is set to the global object ([§10.4.3](https://ecma-international.org/ecma-262/5.1/#sec-10.4.3)).

The reason for writing "in most other cases" is because there are eight ECMAScript 5 built-in functions that allow ThisBinding to be specified in the arguments list. These special functions take a so-called `thisArg` which becomes the ThisBinding when calling the function ([§10.4.3](https://ecma-international.org/ecma-262/5.1/#sec-10.4.3)).

These special built-in functions are:

*   `Function.prototype.apply( thisArg, argArray )`
*   `Function.prototype.call( thisArg [ , arg1 [ , arg2, ... ] ] )`
*   `Function.prototype.bind( thisArg [ , arg1 [ , arg2, ... ] ] )`
*   `Array.prototype.every( callbackfn [ , thisArg ] )`
*   `Array.prototype.some( callbackfn [ , thisArg ] )`
*   `Array.prototype.forEach( callbackfn [ , thisArg ] )`
*   `Array.prototype.map( callbackfn [ , thisArg ] )`
*   `Array.prototype.filter( callbackfn [ , thisArg ] )`

In the case of the `Function.prototype` functions, they are called on a function object, but rather than setting ThisBinding to the function object, ThisBinding is set to the `thisArg`.

In the case of the `Array.prototype` functions, the given `callbackfn` is called in an execution context where ThisBinding is set to `thisArg` if supplied; otherwise, to the global object.

Those are the rules for plain JavaScript. When you begin using JavaScript libraries (e.g. jQuery), you may find that certain library functions manipulate the value of `this`. The developers of those JavaScript libraries do this because it tends to support the most common use cases, and users of the library typically find this behavior to be more convenient. When passing callback functions referencing `this` to library functions, you should refer to the documentation for any guarantees about what the value of `this` is when the function is called.

If you are wondering how a JavaScript library manipulates the value of `this`, the library is simply using one of the built-in JavaScript functions accepting a `thisArg`. You, too, can write your own function taking a callback function and `thisArg`:

    function doWork(callbackfn, thisArg) {
        //...
        if (callbackfn != null) callbackfn.call(thisArg);
    }
    

There’s a special case I didn’t yet mention. When constructing a new object via the `new` operator, the JavaScript interpreter creates a new, empty object, sets some internal properties, and then calls the constructor function on the new object. Thus, when a function is called in a constructor context, the value of `this` is the new object that the interpreter created:

    function MyType() {
        this.someData = "a string";
    }
    
    var instance = new MyType();
    // Kind of like the following, but there are more steps involved:
    // var instance = {};
    // MyType.call(instance);
    

Just for fun, test your understanding with some examples
--------------------------------------------------------

_To reveal the answers, mouse over the light yellow boxes._

1.  What is the value of `this` at the marked line? Why?
    
    > `window` — The marked line is evaluated in the initial global execution context.
    
        if (true) {
            // What is `this` here?
        }
        
    
2.  What is the value of `this` at the marked line when `obj.staticFunction()` is executed? Why?
    
    > `obj` — When calling a function on an object, ThisBinding is set to the object.
    
        var obj = {
            someData: "a string"
        };
        
        function myFun() {
            return this // What is `this` here?
        }
        
        obj.staticFunction = myFun;
        
        console.log("this is window:", obj.staticFunction() == window);
        console.log("this is obj:", obj.staticFunction() == obj);
          
    
3.  What is the value of `this` at the marked line? Why?
    
    > `window`
    > 
    > In this example, the JavaScript interpreter enters function code, but because `myFun`/`obj.myMethod` is not called on an object, ThisBinding is set to `window`.
    > 
    > This is different from Python, in which accessing a method (`obj.myMethod`) creates a [bound method object](http://docs.python.org/3/library/stdtypes.html#methods).
    
        var obj = {
            myMethod: function () {
                return this; // What is `this` here?
            }
        };
        var myFun = obj.myMethod;
        console.log("this is window:", myFun() == window);
        console.log("this is obj:", myFun() == obj);
          
    
4.  What is the value of `this` at the marked line? Why?
    
    > `window`
    > 
    > This one was tricky. When evaluating the eval code, `this` is `obj`. However, in the eval code, `myFun` is not called on an object, so ThisBinding is set to `window` for the call.
    
        function myFun() {
            return this; // What is `this` here?
        }
        var obj = {
            myMethod: function () {
                eval("myFun()");
            }
        };
        
    
5.  What is the value of `this` at the marked line? Why?
    
    > `obj`
    > 
    > The line `myFun.call(obj);` is invoking the special built-in function `Function.prototype.call()`, which accepts `thisArg` as the first argument.
    
        function myFun() {
            return this; // What is `this` here?
        }
        var obj = {
            someData: "a string"
        };
        console.log("this is window:", myFun.call(obj) == window);
        console.log("this is obj:", myFun.call(obj) == obj);
