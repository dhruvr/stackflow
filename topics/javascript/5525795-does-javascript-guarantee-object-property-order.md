
# Does JavaScript Guarantee Object Property Order?

## Question
        
If I create an object like this:

    var obj = {};
    obj.prop1 = "Foo";
    obj.prop2 = "Bar";
    

Will the resulting object _always_ look like this?

    { prop1 : "Foo", prop2 : "Bar" }
    

That is, will the properties be in the same order that I added them?

## Answer
        
No, properties order in objects is not guaranteed in JavaScript; you need to use an `Array`.

Definition of an Object from [ECMAScript Third Edition (pdf)](http://www.ecma-international.org/publications/files/ECMA-ST-ARCH/ECMA-262,%203rd%20edition,%20December%201999.pdf):

> ### 4.3.3 Object
> 
> An object is a member of the type Object. **It is an unordered collection of properties** each of which contains a primitive value, object, or function. A function stored in a property of an object is called a method.

**Since ECMAScript 2015**, using [the `Map` object](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Map) could be an alternative. A `Map` shares some similarities with an `Object` and [guarantees the keys order](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Map#Objects_and_maps_compared):

> A Map iterates its elements in insertion order, whereas iteration order is not specified for Objects.
