
# How can I merge properties of two JavaScript objects dynamically?

## Question
        
I need to be able to merge two (very simple) JavaScript objects at runtime. For example I'd like to:

    var obj1 = { food: 'pizza', car: 'ford' }
    var obj2 = { animal: 'dog' }
    
    obj1.merge(obj2);
    
    //obj1 now has three properties: food, car, and animal
    

Does anyone have a script for this or know of a built in way to do this? I do not need recursion, and I do not need to merge functions, just methods on flat objects.

## Answer
        
**ECMAScript 2018 Standard Method**

You would use [object spread](https://github.com/tc39/proposal-object-rest-spread):

    let merged = {...obj1, ...obj2};
    
    /** There's no limit to the number of objects you can merge.
     *  Later properties overwrite earlier properties with the same name. */
    const allRules = {...obj1, ...obj2, ...obj3};
    

**ECMAScript 2015 (ES6) Standard Method**

    /* For the case in question, you would do: */
    Object.assign(obj1, obj2);
    
    /** There's no limit to the number of objects you can merge.
     *  All objects get merged into the first object. 
     *  Only the object in the first argument is mutated and returned.
     *  Later properties overwrite earlier properties with the same name. */
    const allRules = Object.assign({}, obj1, obj2, obj3, etc);
    

(see [MDN JavaScript Reference](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Object/assign#Browser_compatibility))

* * *

**Method for ES5 and Earlier**

    for (var attrname in obj2) { obj1[attrname] = obj2[attrname]; }
    

Note that this will simply add all attributes of `obj2` to `obj1` which might not be what you want if you still want to use the unmodified `obj1`.

If you're using a framework that craps all over your prototypes then you have to get fancier with checks like `hasOwnProperty`, but that code will work for 99% of cases.

Example function:

    /**
     * Overwrites obj1's values with obj2's and adds obj2's if non existent in obj1
     * @param obj1
     * @param obj2
     * @returns obj3 a new object based on obj1 and obj2
     */
    function merge_options(obj1,obj2){
        var obj3 = {};
        for (var attrname in obj1) { obj3[attrname] = obj1[attrname]; }
        for (var attrname in obj2) { obj3[attrname] = obj2[attrname]; }
        return obj3;
    }
