
# How to correctly curry a function in JavaScript?

## Question
        
I wrote a simple `curry` function in JavaScript which works correctly for most cases:

    var add = curry(function (a, b, c) {
        return a + b + c;
    });
    
    var add2 = add(2);
    
    var add5 = add2(3);
    
    alert(add5(5));

    <script>
    function curry(f) {
        var length = f.length;
    
        if (length > 0) {
            return partial(f, length, []);
        } else {
            return f; // f is already curried
        }
    }
    
    function partial(f, length, a) {
        return function () {
            var arity = length;
            var count = arguments.length;
            var args  = new Array(count);
            var index = 0;
    
            while (index < count) {
                args[index] = arguments[index++];
            }
    
            args = a.concat(args);
    
            return count < arity ?
                partial(f, arity - count, args) :
                f.apply(this, args);
        };
    }
    </script>

However, it doesn't work for the following case:

    // length :: [a] -> Number
    
    function length(a) {
        return a.length;
    }
    
    // filter :: (a -> Bool) -> [a] -> [a]
    
    var filter = curry(function (f, a) {
        return a.filter(f);
    });
    
    // compose :: (b -> c) -> (a -> b) -> a -> c
    
    var compose = curry(function (f, g, x) {
        return f(g(x));
    });
    
    // countWhere :: (a -> Bool) -> [a] -> Number
    
    var countWhere = compose(compose(length), filter);
    

According to the following question `countWhere` is defined as `(length .) . filter`:

[What does (f .) . g mean in Haskell?](https://stackoverflow.com/q/20279306/783743)

Hence I should be able to use `countWhere` as follows:

    countWhere(odd, [1,2,3,4,5]);
    
    function odd(n) {
        return n % 2 === 1;
    }
    

However, instead of returning `3` (the length of the array `[1,3,5]`), it returns a function. What am I doing wrong?

## Answer
        
The problem with your `curry` function (and for [most](http://ramdajs.com/docs/R.html#curry) [`curry`](https://github.com/dominictarr/curry) [functions](http://fitzgen.github.io/wu.js/#curryable) [that](https://lodash.com/docs#curry) [people](http://benalman.com/news/2012/09/partial-application-in-javascript/#currying) [write](http://extralogical.net/articles/currying-javascript.html) in JavaScript) is that it doesn't handle extra arguments correctly.

**What `curry` does**

Suppose `f` is a function and `f.length` is `n`. Let `curry(f)` be `g`. We call `g` with `m` arguments. What should happen?

1.  If `m === 0` then just return `g`.
2.  If `m < n` then partially apply `f` to the `m` new arguments, and return a new curried function which accepts the remaining `n - m` arguments.
3.  Otherwise apply `f` to the `m` arguments and return the result.

This is what most `curry` functions do, and this is wrong. The first two cases are right, but the third case is wrong. Instead, it should be:

1.  If `m === 0` then just return `g`.
2.  If `m < n` then partially apply `f` to the `m` new arguments, and return a new curried function which accepts the remaining `n - m` arguments.
3.  If `m === n` then apply `f` to the `m` arguments. If the result is a function then `curry` the result. Finally, return the result.
4.  If `m > n` then apply `f` to the first `n` arguments. If the result is a function then `curry` the result. Finally, apply the result to the remaining `m - n` arguments and return the new result.

**The problem with most `curry` functions**

Consider the following code:

    var countWhere = compose(compose(length), filter);
    
    countWhere(odd, [1,2,3,4,5]);
    

If we use the incorrect `curry` functions, then this is equivalent to:

    compose(compose(length), filter, odd, [1,2,3,4,5]);
    

However, `compose` only accepts three arguments. The last argument is dropped:

    var compose = curry(function (f, g, x) {
        return f(g(x));
    });
    

Hence, the above expression evaluates to:

    compose(length)(filter(odd));
    

This further evaluates to:

    compose(length, filter(odd));
    

The `compose` function expects one more argument which is why it returns a function instead of returning `3`. To get the correct output you need to write:

    countWhere(odd)([1,2,3,4,5]);
    

This is the reason why most `curry` functions are wrong.

**The solution using the correct `curry` function**

Consider the following code again:

    var countWhere = compose(compose(length), filter);
    
    countWhere(odd, [1,2,3,4,5]);
    

If we use the correct `curry` function, then this is equivalent to:

    compose(compose(length), filter, odd)([1,2,3,4,5]);
    

Which evaluates to:

    compose(length)(filter(odd))([1,2,3,4,5]);
    

Which further evaluates to (skipping an intermediate step):

    compose(length, filter(odd), [1,2,3,4,5]);
    

Which results in:

    length(filter(odd, [1,2,3,4,5]));
    

Producing the correct result `3`.

**The implementation of the correct `curry` function**

Note that I'm not using `slice` to convert the `arguments` object into an array because that is an [optimization killer](https://github.com/petkaantonov/bluebird/wiki/Optimization-killers#32-leaking-arguments) in V8:

    function curry(f) {
        var length = f.length;
        if (length > 0) return partial(f, length, []);
        else return f;  // f is already curried
    }
    
    function partial(f, length, a) {
        return function () {
            var count = arguments.length;
            var arity = length;
            var part  = count < arity;
            var size  = part ? count : arity;
            var args  = new Array(size);
            var index = 0;
    
            while (index < size) args[index] = arguments[index++];
            if (part) return partial(f, arity - count, a.concat(args));
            var result = f.apply(null, a.concat(args));
    
            if (typeof result === "function")
                result = curry(result);
    
            if (arity < count) {
                var args = new Array(count - arity), number = 0;
                while (index < count) args[number++] = arguments[index++];
                return result.apply(null, args);
            }
    
            return result;
        };
    }
    

I am not sure how fast this implementation of `curry` is. Perhaps somebody could make it faster.

**Implications of using the correct `curry` function**

Using the correct `curry` function allows you to directly translate Haskell code into JavaScript. For example:

    var id = curry(function (a) {
        return a;
    });
    
    var flip = curry(function (f, x, y) {
        return f(y, x);
    });
    

The `id` function is useful because it allows you to partially apply a non-curried function easily:

    var add2 = id(add, 2);
    
    function add(a, b) {
        return a + b;
    }
    

The `flip` function is useful because it allows you to easily create [right sections](https://stackoverflow.com/a/25720884/783743) in JavaScript:

    var sub2 = flip(sub, 2); // equivalent to (x - 2)
    
    function sub(a, b) {
        return a - b;
    }
    

It also means that you don't need hacks like this [extended `compose` function](http://ramdajs.com/docs/R.html#useWith):

[What's a Good Name for this extended \`compose\` function?](https://stackoverflow.com/q/17386706/783743)

You can simply write:

    var project = compose(map, pick);
    

As mentioned in the question, if you want to compose `length` and `filter` then you use the `(f .) . g` pattern:

[What does (f .) . g mean in Haskell?](https://stackoverflow.com/q/20279306/783743)

Another solution is to create higher order `compose` functions:

    var compose2 = compose(compose, compose);
    
    var countWhere = compose2(length, fitler);
    

This is all possible because of the correct implementation of the `curry` function.

**Extra food for thought**

I usually use the following `chain` function when I want to compose a chain of functions:

    var chain = compose(function (a, x) {
        var length = a.length;
        while (length > 0) x = a[--length](x);
        return x;
    });
    

This allows you to write code like:

    var inc = add(1);
    
    var foo = chain([map(inc), filter(odd), take(5)]);
    
    foo([1,2,3,4,5,6,7,8,9,10]); // [2,4,6]
    

Which is equivalent to the following Haskell code:

    let foo = map (+1) . filter odd . take 5
    
    foo [1,2,3,4,5,6,7,8,9,10]
    

It also allows you to write code like:

    chain([map(inc), filter(odd), take(5)], [1,2,3,4,5,6,7,8,9,10]); // [2,4,6]
    

Which is equivalent to the following Haskell code:

    map (+1) . filter odd . take 5 $ [1,2,3,4,5,6,7,8,9,10]
    

Hope that helps.
