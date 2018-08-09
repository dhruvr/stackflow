
# For-each over an array in JavaScript?

## Question
        
How can I loop through all the entries in an array using JavaScript?

I thought it was something like this:

    forEach(instance in theArray)
    

Where `theArray` is my array, but this seems to be incorrect.

## Answer
        
**TL;DR**

*   Don't use `for-in` unless you use it with safeguards or are at least aware of why it might bite you.
*   Your best bets are usually
    
    *   a `for-of` loop (ES2015+ only),
    *   `Array#forEach` ([`spec`](https://tc39.github.io/ecma262/#sec-array.prototype.foreach) | [`MDN`](https://developer.mozilla.org/docs/Web/JavaScript/Reference/Global_Objects/Array/forEach)) (or its relatives `some` and such) (ES5+ only),
    *   a simple old-fashioned `for` loop,
    *   or `for-in` with safeguards.

But there's **lots** more to explore, read on...

* * *

JavaScript has powerful semantics for looping through arrays and array-like objects. I've split the answer into two parts: Options for genuine arrays, and options for things that are just array-_like_, such as the `arguments` object, other iterable objects (ES2015+), DOM collections, and so on.

I'll quickly note that you can use the ES2015 options _now_, even on ES5 engines, by _transpiling_ ES2015 to ES5. Search for "ES2015 transpiling" / "ES6 transpiling" for more...

Okay, let's look at our options:

For Actual Arrays
-----------------

You have three options in [ECMAScript 5](http://ecma-international.org/ecma-262/5.1/) ("ES5"), the version most broadly supported at the moment, and two more added in [ECMAScript 2015](http://www.ecma-international.org/ecma-262/6.0/index.html) ("ES2015", "ES6"):

1.  Use `forEach` and related (ES5+)
2.  Use a simple `for` loop
3.  Use `for-in` _correctly_
4.  Use `for-of` (use an iterator implicitly) (ES2015+)
5.  Use an iterator explicitly (ES2015+)

Details:

### 1\. Use `forEach` and related

In any vaguely-modern environment (so, not IE8) where you have access to the `Array` features added by ES5 (directly or using polyfills), you can use `forEach` ([`spec`](https://tc39.github.io/ecma262/#sec-array.prototype.foreach) | [`MDN`](https://developer.mozilla.org/docs/Web/JavaScript/Reference/Global_Objects/Array/forEach)):

    var a = ["a", "b", "c"];
    a.forEach(function(entry) {
        console.log(entry);
    });
    

`forEach` accepts a callback function and, optionally, a value to use as `this` when calling that callback (not used above). The callback is called for each entry in the array, in order, skipping non-existent entries in sparse arrays. Although I only used one argument above, the callback is called with three: The value of each entry, the index of that entry, and a reference to the array you're iterating over (in case your function doesn't already have it handy).

Unless you're supporting obsolete browsers like IE8 (which NetApps shows at just over 4% market share as of this writing in September 2016), you can happily use `forEach` in a general-purpose web page without a shim. If you do need to support obsolete browsers, shimming/polyfilling `forEach` is easily done (search for "es5 shim" for several options).

`forEach` has the benefit that you don't have to declare indexing and value variables in the containing scope, as they're supplied as arguments to the iteration function, and so nicely scoped to just that iteration.

If you're worried about the runtime cost of making a function call for each array entry, don't be; [details](http://blog.niftysnippets.org/2012/02/foreach-and-runtime-cost.html).

Additionally, `forEach` is the "loop through them all" function, but ES5 defined several other useful "work your way through the array and do things" functions, including:

*   [`every`](https://tc39.github.io/ecma262/#sec-array.prototype.every) (stops looping the first time the callback returns `false` or something falsey)
*   [`some`](https://tc39.github.io/ecma262/#sec-array.prototype.some) (stops looping the first time the callback returns `true` or something truthy)
*   [`filter`](https://tc39.github.io/ecma262/#sec-array.prototype.filter) (creates a new array including elements where the filter function returns `true` and omitting the ones where it returns `false`)
*   [`map`](https://tc39.github.io/ecma262/#sec-array.prototype.map) (creates a new array from the values returned by the callback)
*   [`reduce`](https://tc39.github.io/ecma262/#sec-array.prototype.reduce) (builds up a value by repeatedly calling the callback, passing in previous values; see the spec for the details; useful for summing the contents of an array and many other things)
*   [`reduceRight`](https://tc39.github.io/ecma262/#sec-array.prototype.reduceright) (like `reduce`, but works in descending rather than ascending order)

### 2\. Use a simple `for` loop

Sometimes the old ways are the best:

    var index;
    var a = ["a", "b", "c"];
    for (index = 0; index < a.length; ++index) {
        console.log(a[index]);
    }
    

If the length of the array won't change during the loop, and it's in performance-sensitive code (unlikely), a slightly more complicated version grabbing the length up front might be a **_tiny_** bit faster:

    var index, len;
    var a = ["a", "b", "c"];
    for (index = 0, len = a.length; index < len; ++index) {
        console.log(a[index]);
    }
    

And/or counting backward:

    var index;
    var a = ["a", "b", "c"];
    for (index = a.length - 1; index >= 0; --index) {
        console.log(a[index]);
    }
    

But with modern JavaScript engines, it's rare you need to eke out that last bit of juice.

In ES2015 and higher, you can make your index and value variables local to the `for` loop:

    let a = ["a", "b", "c"];
    for (let index = 0; index < a.length; ++index) {
        let value = a[index];
    }
    //console.log(index); // Would cause "ReferenceError: index is not defined"
    //console.log(value); // Would cause "ReferenceError: value is not defined"
    

And when you do that, not just `value` but also `index` is recreated for each loop iteration, meaning closures created in the loop body keep a reference to the `index` (and `value`) created for that specific iteration:

    let divs = document.querySelectorAll("div");
    for (let index = 0; index < divs.length; ++index) {
        divs[index].addEventListener('click', e => {
            alert("Index is: " + index);
        });
    }
    

If you had five divs, you'd get "Index is: 0" if you clicked the first and "Index is: 4" if you clicked the last. This does **not** work if you use `var` instead of `let`.

### 3\. Use `for-in` _correctly_

You'll get people telling you to use `for-in`, but [that's not what `for-in` is for](http://blog.niftysnippets.org/2010/11/myths-and-realities-of-forin.html). `for-in` loops through the _enumerable properties of an object_, not the indexes of an array. **The order is not guaranteed**, not even in ES2015 (ES6). ES2015 does define an order to object properties (via [`[[OwnPropertyKeys]]`](https://tc39.github.io/ecma262/#sec-ordinary-object-internal-methods-and-internal-slots-ownpropertykeys), [`[[Enumerate]]`](https://tc39.github.io/ecma262/#sec-ordinary-object-internal-methods-and-internal-slots-enumerate), and things that use them like [`Object.getOwnPropertyKeys`](https://tc39.github.io/ecma262/#sec-object.getownpropertynames)), but it **does not** define that `for-in` will follow that order. (Details in [this other answer](https://stackoverflow.com/a/30919039/157247).)

Still, it _can_ be useful, particularly for [_sparse_ arrays](http://en.wikipedia.org/wiki/Sparse_array), if you use appropriate safeguards:

    // `a` is a sparse array
    var key;
    var a = [];
    a[0] = "a";
    a[10] = "b";
    a[10000] = "c";
    for (key in a) {
        if (a.hasOwnProperty(key)  &&        // These are explained
            /^0$|^[1-9]\d*$/.test(key) &&    // and then hidden
            key <= 4294967294                // away below
            ) {
            console.log(a[key]);
        }
    }
    

Note the two checks:

1.  That the object has its _own_ property by that name (not one it inherits from its prototype), and
    
2.  That the key is a base-10 numeric string in its normal string form and its value is <= 2^32 - 2 (which is 4,294,967,294). Where does that number come from? It's part of the definition of an array index [in the specification](https://tc39.github.io/ecma262/#array-index). Other numbers (non-integers, negative numbers, numbers greater than 2^32 - 2) are not array indexes. The reason it's 2^32 - **2** is that that makes the greatest index value one lower than 2^32 - **1**, which is the maximum value an array's `length` can have. (E.g., an array's length fits in a 32-bit unsigned integer.) _(Props to RobG for pointing out in a comment [on my blog post](http://blog.niftysnippets.org/2010/11/myths-and-realities-of-forin.html) that my previous test wasn't quite right.)_
    

That's a tiny bit of added overhead per loop iteration on most arrays, but if you have a _sparse_ array, it can be a more efficient way to loop because it only loops for entries that actually exist. E.g., for the array above, we loop a total of three times (for keys `"0"`, `"10"`, and `"10000"` — remember, they're strings), not 10,001 times.

Now, you won't want to write that every time, so you might put this in your toolkit:

    function arrayHasOwnIndex(array, prop) {
        return array.hasOwnProperty(prop) && /^0$|^[1-9]\d*$/.test(prop) && prop <= 4294967294; // 2^32 - 2
    }
    

And then we'd use it like this:

    for (key in a) {
        if (arrayHasOwnIndex(a, key)) {
            console.log(a[key]);
        }
    }
    

Or if you're interested in just a "good enough for most cases" test, you could use this, but while it's close, it's not quite correct:

    for (key in a) {
        // "Good enough" for most cases
        if (String(parseInt(key, 10)) === key && a.hasOwnProperty(key)) {
            console.log(a[key]);
        }
    }
    

### 4\. Use `for-of` (use an iterator implicitly) (ES2015+)

ES2015 adds _iterators_ to JavaScript. The easiest way to use iterators is the new `for-of` statement. It looks like this:

    var val;
    var a = ["a", "b", "c"];
    for (val of a) {
        console.log(val);
    }
    

Output:

a
b
c

Under the covers, that gets an _iterator_ from the array and loops through it, getting the values from it. This doesn't have the issue that using `for-in` has, because it uses an iterator defined by the object (the array), and arrays define that their iterators iterate through their _entries_ (not their properties). Unlike `for-in` in ES5, the order in which the entries are visited is the numeric order of their indexes.

### 5\. Use an iterator explicitly (ES2015+)

Sometimes, you might want to use an iterator _explicitly_. You can do that, too, although it's a lot clunkier than `for-of`. It looks like this:

    var a = ["a", "b", "c"];
    var it = a.values();
    var entry;
    while (!(entry = it.next()).done) {
        console.log(entry.value);
    }
    

The iterator is an object matching the Iterator definition in the specification. Its `next` method returns a new _result object_ each time you call it. The result object has a property, `done`, telling us whether it's done, and a property `value` with the value for that iteration. (`done` is optional if it would be `false`, `value` is optional if it would be `undefined`.)

The meaning of `value` varies depending on the iterator; arrays support (at least) three functions that return iterators:

*   `values()`: This is the one I used above. It returns an iterator where each `value` is the array entry for that iteration (`"a"`, `"b"`, and `"c"` in the example earlier).
*   `keys()`: Returns an iterator where each `value` is the key for that iteration (so for our `a` above, that would be `"0"`, then `"1"`, then `"2"`).
*   `entries()`: Returns an iterator where each `value` is an array in the form `[key, value]` for that iteration.

For Array-Like Objects
----------------------

Aside from true arrays, there are also _array-like_ objects that have a `length` property and properties with numeric names: `NodeList` instances, the `arguments` object, etc. How do we loop through their contents?

### Use any of the options above for arrays

At least some, and possibly most or even all, of the array approaches above frequently apply equally well to array-like objects:

1.  **Use `forEach` and related (ES5+)**
    
    The various functions on `Array.prototype` are "intentionally generic" and can usually be used on array-like objects via [`Function#call`](https://tc39.github.io/ecma262/#sec-function.prototype.call) or [`Function#apply`](https://tc39.github.io/ecma262/#sec-function.prototype.apply). (See the _Caveat for host-provided objects_ at the end of this answer, but it's a rare issue.)
    
    Suppose you wanted to use `forEach` on a `Node`'s `childNodes` property. You'd do this:
    
        Array.prototype.forEach.call(node.childNodes, function(child) {
            // Do something with `child`
        });
        
    
    If you're going to do that a lot, you might want to grab a copy of the function reference into a variable for reuse, e.g.:
    
        // (This is all presumably in some scoping function)
        var forEach = Array.prototype.forEach;
        
        // Then later...
        forEach.call(node.childNodes, function(child) {
            // Do something with `child`
        });
        
    
2.  **Use a simple `for` loop**
    
    Obviously, a simple `for` loop applies to array-like objects.
    
3.  **Use `for-in` _correctly_**
    
    `for-in` with the same safeguards as with an array should work with array-like objects as well; the caveat for host-provided objects on #1 above may apply.
    
4.  **Use `for-of` (use an iterator implicitly) (ES2015+)**
    
    `for-of` will use the iterator provided by the object (if any); we'll have to see how this plays with the various array-like objects, particularly host-provided ones. For instance, the specification for the `NodeList` from `querySelectorAll` was updated to support iteration. The spec for the `HTMLCollection` from `getElementsByTagName` was not.
    
5.  **Use an iterator explicitly (ES2015+)**
    
    See #4, we'll have to see how iterators play out.
    

### Create a true array

Other times, you may want to convert an array-like object into a true array. Doing that is surprisingly easy:

1.  **Use the [`slice`](https://tc39.github.io/ecma262/#sec-array.prototype.slice) method of arrays**
    
    We can use the `slice` method of arrays, which like the other methods mentioned above is "intentionally generic" and so can be used with array-like objects, like this:
    
        var trueArray = Array.prototype.slice.call(arrayLikeObject);
        
    
    So for instance, if we want to convert a `NodeList` into a true array, we could do this:
    
        var divs = Array.prototype.slice.call(document.querySelectorAll("div"));
        
    
    See the _Caveat for host-provided objects_ below. In particular, note that this will fail in IE8 and earlier, which don't let you use host-provided objects as `this` like that.
    
2.  **Use [spread syntax (`...`)](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Spread_syntax)**
    
    It's also possible to use ES2015's [spread syntax](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Spread_syntax) with JavaScript engines that support this feature:
    
        var trueArray = [...iterableObject];
        
    
    So for instance, if we want to convert a `NodeList` into a true array, with spread syntax this becomes quite succinct:
    
        var divs = [...document.querySelectorAll("div")];
        
    
3.  **Use `Array.from`** [(spec)](https://tc39.github.io/ecma262/#sec-array.from) | [(MDN)](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/from)
    
    `Array.from` (ES2015+, but easily polyfilled) creates an array from an array-like object, optionally passing the entries through a mapping function first. So:
    
        var divs = Array.from(document.querySelectorAll("div"));
        
    
    Or if you wanted to get an array of the tag names of the elements with a given class, you'd use the mapping function:
    
        // Arrow function (ES2015):
        var divs = Array.from(document.querySelectorAll(".some-class"), element => element.tagName);
        
        // Standard function (since `Array.from` can be shimmed):
        var divs = Array.from(document.querySelectorAll(".some-class"), function(element) {
            return element.tagName;
        });
        
    

### Caveat for host-provided objects

If you use `Array.prototype` functions with _host-provided_ array-like objects (DOM lists and other things provided by the browser rather than the JavaScript engine), you need to be sure to test in your target environments to make sure the host-provided object behaves properly. **Most do behave properly** (now), but it's important to test. The reason is that most of the `Array.prototype` methods you're likely to want to use rely on the host-provided object giving an honest answer to the abstract [`[[HasProperty]]`](https://tc39.github.io/ecma262/#sec-ordinary-object-internal-methods-and-internal-slots-hasproperty-p) operation. As of this writing, browsers do a very good job of this, but the 5.1 spec did allow for the possibility a host-provided object may not be honest. It's in [§8.6.2](http://www.ecma-international.org/ecma-262/5.1/#sec-8.6.2), several paragraphs below the big table near the beginning of that section), where it says:

> Host objects may implement these internal methods in any manner unless specified otherwise; for example, one possibility is that `[[Get]]` and `[[Put]]` for a particular host object indeed fetch and store property values but `[[HasProperty]]` always generates **false**.

(I couldn't find the equivalent verbiage in the ES2015 spec, but it's bound to still be the case.) Again, as of this writing the common host-provided array-like objects in modern browsers \[`NodeList` instances, for instance\] **do** handle `[[HasProperty]]` correctly, but it's important to test.)
