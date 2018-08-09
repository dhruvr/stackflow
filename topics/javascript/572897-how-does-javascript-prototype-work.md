
# How does JavaScript .prototype work?

## Question
        
I'm not that into dynamic programming languages, but I've written my fair share of JavaScript code. I never really got my head around this prototype-based programming, does any one know how this works?

    var obj = new Object(); // not a functional object
    obj.prototype.test = function() { alert('Hello?'); }; // this is wrong!
    
    function MyObject() {} // a first class functional object
    MyObject.prototype.test = function() { alert('OK'); } // OK
    

I remember a lot discussion I had with people a while back (I'm not exactly sure what I'm doing) but as I understand it, there's no concept of a class. It's just an object, and instances of those objects are clones of the original, right?

But what is the exact purpose of this `.prototype` property in JavaScript? How does it relate to instantiating objects?

* * *

### Edit

These [slides](http://ejohn.org/apps/learn/#64) really helped a lot to understand this topic.

## Answer
        
Every JavaScript object has an internal property called _\[\[Prototype\]\]_. If you look up a property via `obj.propName` or `obj['propName']` and the object does not have such a property - which can be checked via `obj.hasOwnProperty('propName')` \- the runtime looks up the property in the object referenced by \[\[Prototype\]\] instead. If the prototype-object also doesn't have such a property, its prototype is checked in turn, thus walking the original object's _prototype-chain_ until a match is found or its end is reached.

Some JavaScript implementations allow direct access to the \[\[Prototype\]\] property, eg via a non-standard property named `__proto__`. In general, it's only possible to set an object's prototype during object creation: If you create a new object via `new Func()`, the object's \[\[Prototype\]\] property will be set to the object referenced by `Func.prototype`.

This allows to simulate classes in JavaScript, although JavaScript's inheritance system is - as we have seen - prototypical, and not class-based:

Just think of constructor functions as classes and the properties of the prototype (ie of the object referenced by the constructor function's `prototype` property) as shared members, ie members which are the same for each instance. In class-based systems, methods are implemented the same way for each instance, so methods are normally added to the prototype, whereas an object's fields are instance-specific and therefore added to the object itself during construction.
