
# Access / process (nested) objects, arrays or JSON

## Question
        
I have a nested data structure containing objects and arrays. How can I extract the information, i.e. access a specific or multiple values (or keys)?

For example:

    var data = {
        code: 42,
        items: [{
            id: 1,
            name: 'foo'
        }, {
            id: 2,
            name: 'bar'
        }]
    };
    

How could I access the `name` of the second item in `items`?

## Answer
        
Preliminaries
-------------

JavaScript has only one data type which can contain multiple values: **Object**. An **Array** is a special form of object.

(Plain) Objects have the form

    {key: value, key: value, ...}
    

Arrays have the form

    [value, value, ...]
    

Both arrays and objects expose a `key -> value` structure. Keys in an array must be numeric, whereas any string can be used as key in objects. The key-value pairs are also called the **"properties"**.

Properties can be accessed either using **dot notation**

    const value = obj.someProperty;
    

or **bracket notation**, if the property name would not be a valid JavaScript [identifier name _\[spec\]_](http://es5.github.com/#x7.6), or the name is the value of a variable:

    // the space is not a valid character in identifier names
    const value = obj["some Property"];
    
    // property name as variable
    const name = "some Property";
    const value = obj[name];
    

For that reason, array elements can only be accessed using bracket notation:

    const value = arr[5]; // arr.5 would be a syntax error
    
    // property name / index as variable
    const x = 5;
    const value = arr[x];
    

### Wait... what about JSON?

JSON is a textual representation of data, just like XML, YAML, CSV, and others. To work with such data, it first has to be converted to JavaScript data types, i.e. arrays and objects (and how to work with those was just explained). How to parse JSON is explained in the question [Parse JSON in JavaScript?](https://stackoverflow.com/questions/4935632/how-to-parse-json-in-javascript) .

### Further reading material

How to access arrays and objects is fundamental JavaScript knowledge and therefore it is advisable to read the [MDN JavaScript Guide](https://developer.mozilla.org/en-US/docs/JavaScript/Guide), especially the sections

*   [Working with Objects](https://developer.mozilla.org/en-US/docs/JavaScript/Guide/Working_with_Objects)
*   [Arrays](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Indexed_collections#Array_object)
*   [Eloquent JavaScript - Data Structures](http://eloquentjavascript.net/04_data.html)

* * *

* * *

Accessing nested data structures
--------------------------------

A nested data structure is an array or object which refers to other arrays or objects, i.e. its values are arrays or objects. Such structures can be accessed by consecutively applying dot or bracket notation.

Here is an example:

    const data = {
        code: 42,
        items: [{
            id: 1,
            name: 'foo'
        }, {
            id: 2,
            name: 'bar'
        }]
    };
    

Let's assume we want to access the `name` of the second item.

Here is how we can do it step-by-step:

As we can see `data` is an object, hence we can access its properties using dot notation. The `items` property is accessed as follows:

    data.items
    

The value is an array, to access its second element, we have to use bracket notation:

    data.items[1]
    

This value is an object and we use dot notation again to access the `name` property. So we eventually get:

    const item_name = data.items[1].name;
    

Alternatively, we could have used bracket notation for any of the properties, especially if the name contained characters that would have made it invalid for dot notation usage:

    const item_name = data['items'][1]['name'];
    

* * *

### I'm trying to access a property but I get only `undefined` back?

Most of the time when you are getting `undefined`, the object/array simply doesn't have a property with that name.

    const foo = {bar: {baz: 42}};
    console.log(foo.baz); // undefined
    

Use [`console.log`](https://developer.mozilla.org/en-US/docs/DOM/console.log) or [`console.dir`](https://developer.mozilla.org/en-US/docs/DOM/console.dir) and inspect the structure of object / array. The property you are trying to access might be actually defined on a nested object / array.

    console.log(foo.bar.baz); // 42
    

* * *

### What if the property names are dynamic and I don't know them beforehand?

If the property names are unknown or we want to access all properties of an object / elements of an array, we can use the [`for...in` _\[MDN\]_](https://developer.mozilla.org/en-US/docs/JavaScript/Reference/Statements/for...in) loop for objects and the [`for` _\[MDN\]_](https://developer.mozilla.org/en-US/docs/JavaScript/Reference/Statements/for) loop for arrays to iterate over all properties / elements.

**Objects**

To iterate over all properties of `data`, we can iterate over the **object** like so:

    for (const prop in data) {
        // `prop` contains the name of each property, i.e. `'code'` or `'items'`
        // consequently, `data[prop]` refers to the value of each property, i.e.
        // either `42` or the array
    }
    

Depending on where the object comes from (and what you want to do), you might have to test in each iteration whether the property is really a property of the object, or it is an inherited property. You can do this with [`Object#hasOwnProperty` _\[MDN\]_](https://developer.mozilla.org/en-US/docs/JavaScript/Reference/Global_Objects/Object/hasOwnProperty).

As alternative to `for...in` with `hasOwnProperty`, you can use [`Object.keys` _\[MDN\]_](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Object/keys) to get an _array of property names_:

    Object.keys(data).forEach(function(prop) {
      // `prop` is the property name
      // `data[prop]` is the property value
    });
    

**Arrays**

To iterate over all elements of the `data.items` **array**, we use a `for` loop:

    for(let i = 0, l = data.items.length; i < l; i++) {
        // `i` will take on the values `0`, `1`, `2`,..., i.e. in each iteration
        // we can access the next element in the array with `data.items[i]`, example:
        // 
        // var obj = data.items[i];
        // 
        // Since each element is an object (in our example),
        // we can now access the objects properties with `obj.id` and `obj.name`. 
        // We could also use `data.items[i].id`.
    }
    

One could also use `for...in` to iterate over arrays, but there are reasons why this should be avoided: [Why is 'for(var item in list)' with arrays considered bad practice in JavaScript?](https://stackoverflow.com/questions/2265167/why-is-forvar-item-in-list-with-arrays-considered-bad-practice-in-javascript).

With the increasing browser support of ECMAScript 5, the array method [`forEach` _\[MDN\]_](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/forEach) becomes an interesting alternative as well:

    data.items.forEach(function(value, index, array) {
        // The callback is executed for each element in the array.
        // `value` is the element itself (equivalent to `array[index]`)
        // `index` will be the index of the element in the array
        // `array` is a reference to the array itself (i.e. `data.items` in this case)
    }); 
    

In environments supporting ES2015 (ES6), you can also use the [_`for...of`_ _\[MDN\]_](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/for...of) loop, which not only works for arrays, but for any [_iterable_](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Iteration_protocols#iterable):

    for (const item of data.items) {
       // `item` is the array element, **not** the index
    }
    

In each iteration, `for...of` directly gives us the next element of the iterable, there is no "index" to access or use.

* * *

### What if the "depth" of the data structure is unknown to me?

In addition to unknown keys, the "depth" of the data structure (i.e. how many nested objects) it has, might be unknown as well. How to access deeply nested properties usually depends on the exact data structure.

But if the data structure contains repeating patterns, e.g. the representation of a binary tree, the solution typically includes to [**recursively** _\[Wikipedia\]_](https://en.wikipedia.org/wiki/Recursion_%28computer_science%29) access each level of the data structure.

Here is an example to get the first leaf node of a binary tree:

    function getLeaf(node) {
        if (node.leftChild) {
            return getLeaf(node.leftChild); // <- recursive call
        }
        else if (node.rightChild) {
            return getLeaf(node.rightChild); // <- recursive call
        }
        else { // node must be a leaf node
            return node;
        }
    }
    
    const first_leaf = getLeaf(root);
    

    const root = {
        leftChild: {
            leftChild: {
                leftChild: null,
                rightChild: null,
                data: 42
            },
            rightChild: {
                leftChild: null,
                rightChild: null,
                data: 5
            }
        },
        rightChild: {
            leftChild: {
                leftChild: null,
                rightChild: null,
                data: 6
            },
            rightChild: {
                leftChild: null,
                rightChild: null,
                data: 7
            }
        }
    };
    function getLeaf(node) {
        if (node.leftChild) {
            return getLeaf(node.leftChild);
        } else if (node.rightChild) {
            return getLeaf(node.rightChild);
        } else { // node must be a leaf node
            return node;
        }
    }
    
    console.log(getLeaf(root).data);

A more generic way to access a nested data structure with unknown keys and depth is to test the type of the value and act accordingly.

Here is an example which adds all primitive values inside a nested data structure into an array (assuming it does not contain any functions). If we encounter an object (or array) we simply call `toArray` again on that value (recursive call).

    function toArray(obj) {
        const result = [];
        for (const prop in obj) {
            const value = obj[prop];
            if (typeof value === 'object') {
                result.push(toArray(value)); // <- recursive call
            }
            else {
                result.push(value);
            }
        }
        return result;
    }
    

    const data = {
      code: 42,
      items: [{
        id: 1,
        name: 'foo'
      }, {
        id: 2,
        name: 'bar'
      }]
    };
    
    
    function toArray(obj) {
      const result = [];
      for (const prop in obj) {
        const value = obj[prop];
        if (typeof value === 'object') {
          result.push(toArray(value));
        } else {
          result.push(value);
        }
      }
      return result;
    }
    
    console.log(toArray(data));

* * *

* * *

Helpers
-------

Since the structure of a complex object or array is not necessarily obvious, we can inspect the value at each step to decide how to move further. [`console.log` _\[MDN\]_](https://developer.mozilla.org/en-US/docs/DOM/console.log) and [`console.dir` _\[MDN\]_](https://developer.mozilla.org/en-US/docs/DOM/console.dir) help us doing this. For example (output of the Chrome console):

    > console.log(data.items)
     [ Object, Object ]
    

Here we see that that `data.items` is an array with two elements which are both objects. In Chrome console the objects can even be expanded and inspected immediately.

    > console.log(data.items[1])
      Object
         id: 2
         name: "bar"
         __proto__: Object
    

This tells us that `data.items[1]` is an object, and after expanding it we see that it has three properties, `id`, `name` and `__proto__`. The latter is an internal property used for the prototype chain of the object. The prototype chain and inheritance is out of scope for this answer, though.
