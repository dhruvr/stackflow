
# How do I remove a particular element from an array in JavaScript?

## Question
        
I have an array of integers, and I'm using the `.push()` method to add elements to it.

Is there a simple way to remove a specific element from an array? The equivalent of something like `array.remove(int);`.

I have to use _core_ JavaScript - _no_ frameworks are allowed.

## Answer
        
Find the `index` of the array element you want to remove, then remove that index with [`splice`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/splice).

> The splice() method changes the contents of an array by removing existing elements and/or adding new elements.

    var array = [2, 5, 9];
    console.log(array)
    var index = array.indexOf(5);
    if (index > -1) {
      array.splice(index, 1);
    }
    // array = [2, 9]
    console.log(array);

The second parameter of `splice` is the number of elements to remove. Note that `splice` modifies the array in place and returns a new array containing the elements that have been removed.

* * *

**_Note_**_: [browser support for indexOf](http://kangax.github.io/compat-table/es5/#test-Array.prototype.indexOf) is limited_; it is not supported in Internet Explorer 7 and 8.

If you need `indexOf` in an unsupported browser, try the following `polyfill`. Find more info about this [**_`polyfill` here_**](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/indexOf#Polyfill).

    Array.prototype.indexOf || (Array.prototype.indexOf = function(d, e) {
        var a;
        if (null == this) throw new TypeError('"this" is null or not defined');
        var c = Object(this),
            b = c.length >>> 0;
        if (0 === b) return -1;
        a = +e || 0;
        Infinity === Math.abs(a) && (a = 0);
        if (a >= b) return -1;
        for (a = Math.max(0 <= a ? a : b - Math.abs(a), 0); a < b;) {
            if (a in c && c[a] === d) return a;
            a++
        }
        return -1
    });
