
# How do I correctly clone a JavaScript object?

## Question
        
I have an object, `x`. I'd like to copy it as object `y`, such that changes to `y` do not modify `x`. I realized that copying objects derived from built-in JavaScript objects will result in extra, unwanted properties. This isn't a problem, since I'm copying one of my own, literal-constructed objects.

How do I correctly clone a JavaScript object?

## Answer
        
Updated answer
==============

Just use [Object.assign()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Object/assign) as suggested [here](https://stackoverflow.com/a/30042948/4031815)

But be aware that this makes a shallow copy only. Nested objects are still copied as references.

* * *

Outdated answer
===============

To do this for any object in JavaScript will not be simple or straightforward. You will run into the problem of erroneously picking up attributes from the object's prototype that should be left in the prototype and not copied to the new instance. If, for instance, you are adding a `clone` method to `Object.prototype`, as some answers depict, you will need to explicitly skip that attribute. But what if there are other additional methods added to `Object.prototype`, or other intermediate prototypes, that you don't know about? In that case, you will copy attributes you shouldn't, so you need to detect unforeseen, non-local attributes with the [`hasOwnProperty`](https://developer.mozilla.org/en/Core_JavaScript_1.5_Reference/Global_Objects/Object/hasOwnProperty "Mozilla JavaScript Reference: Object.hasOwnProperty") method.

In addition to non-enumerable attributes, you'll encounter a tougher problem when you try to copy objects that have hidden properties. For example, `prototype` is a hidden property of a function. Also, an object's prototype is referenced with the attribute `__proto__`, which is also hidden, and will not be copied by a for/in loop iterating over the source object's attributes. I think `__proto__` might be specific to Firefox's JavaScript interpreter and it may be something different in other browsers, but you get the picture. Not everything is enumerable. You can copy a hidden attribute if you know its name, but I don't know of any way to discover it automatically.

Yet another snag in the quest for an elegant solution is the problem of setting up the prototype inheritance correctly. If your source object's prototype is `Object`, then simply creating a new general object with `{}` will work, but if the source's prototype is some descendant of `Object`, then you are going to be missing the additional members from that prototype which you skipped using the `hasOwnProperty` filter, or which were in the prototype, but weren't enumerable in the first place. One solution might be to call the source object's `constructor` property to get the initial copy object and then copy over the attributes, but then you still will not get non-enumerable attributes. For example, a [`Date`](https://developer.mozilla.org/en/Core_JavaScript_1.5_Reference/Global_Objects/Date "Mozilla JavaScript Reference: Date") object stores its data as a hidden member:

    function clone(obj) {
        if (null == obj || "object" != typeof obj) return obj;
        var copy = obj.constructor();
        for (var attr in obj) {
            if (obj.hasOwnProperty(attr)) copy[attr] = obj[attr];
        }
        return copy;
    }
    
    var d1 = new Date();
    
    /* Executes function after 5 seconds. */
    setTimeout(function(){
        var d2 = clone(d1);
        alert("d1 = " + d1.toString() + "\nd2 = " + d2.toString());
    }, 5000);
    

The date string for `d1` will be 5 seconds behind that of `d2`. A way to make one `Date` the same as another is by calling the `setTime` method, but that is specific to the `Date` class. I don't think there is a bullet-proof general solution to this problem, though I would be happy to be wrong!

When I had to implement general deep copying I ended up compromising by assuming that I would only need to copy a plain `Object`, `Array`, `Date`, `String`, `Number`, or `Boolean`. The last 3 types are immutable, so I could perform a shallow copy and not worry about it changing. I further assumed that any elements contained in `Object` or `Array` would also be one of the 6 simple types in that list. This can be accomplished with code like the following:

    function clone(obj) {
        var copy;
    
        // Handle the 3 simple types, and null or undefined
        if (null == obj || "object" != typeof obj) return obj;
    
        // Handle Date
        if (obj instanceof Date) {
            copy = new Date();
            copy.setTime(obj.getTime());
            return copy;
        }
    
        // Handle Array
        if (obj instanceof Array) {
            copy = [];
            for (var i = 0, len = obj.length; i < len; i++) {
                copy[i] = clone(obj[i]);
            }
            return copy;
        }
    
        // Handle Object
        if (obj instanceof Object) {
            copy = {};
            for (var attr in obj) {
                if (obj.hasOwnProperty(attr)) copy[attr] = clone(obj[attr]);
            }
            return copy;
        }
    
        throw new Error("Unable to copy obj! Its type isn't supported.");
    }
    

The above function will work adequately for the 6 simple types I mentioned, as long as the data in the objects and arrays form a tree structure. That is, there isn't more than one reference to the same data in the object. For example:

    // This would be cloneable:
    var tree = {
        "left"  : { "left" : null, "right" : null, "data" : 3 },
        "right" : null,
        "data"  : 8
    };
    
    // This would kind-of work, but you would get 2 copies of the 
    // inner node instead of 2 references to the same copy
    var directedAcylicGraph = {
        "left"  : { "left" : null, "right" : null, "data" : 3 },
        "data"  : 8
    };
    directedAcyclicGraph["right"] = directedAcyclicGraph["left"];
    
    // Cloning this would cause a stack overflow due to infinite recursion:
    var cyclicGraph = {
        "left"  : { "left" : null, "right" : null, "data" : 3 },
        "data"  : 8
    };
    cyclicGraph["right"] = cyclicGraph;
    

It will not be able to handle any JavaScript object, but it may be sufficient for many purposes as long as you don't assume that it will just work for anything you throw at it.
