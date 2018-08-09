
# What is the explicit promise construction antipattern and how do I avoid it?

## Question
        
I was writing code that does something that looks like:

    function getStuffDone(param) {           | function getStuffDone(param) {
        var d = Q.defer(); /* or $q.defer */ |     return new Promise(function(resolve, reject) {
        // or = new $.Deferred() etc.        |     // using a promise constructor
        myPromiseFn(param+1)                 |         myPromiseFn(param+1)
        .then(function(val) { /* or .done */ |         .then(function(val) {
            d.resolve(val);                  |             resolve(val);
        }).catch(function(err) { /* .fail */ |         }).catch(function(err) {
            d.reject(err);                   |             reject(err);
        });                                  |         });
        return d.promise; /* or promise() */ |     });
    }                                        | }
    

Someone told me this is called the "**deferred antipattern**" or the "**`Promise` constructor antipattern**" respectively, what's bad about this code and why is this called an [antipattern](https://en.wikipedia.org/wiki/Anti-pattern)?

## Answer
        
The [deferred antipattern (now explicit-construction anti-pattern)](https://github.com/petkaantonov/bluebird/wiki/Promise-anti-patterns#the-deferred-anti-pattern) coined by [Esailija](https://stackoverflow.com/users/995876/esailija) is a common anti-pattern people who are new to promises make, I've made it myself when I first used promises. The problem with the above code is that is fails to utilize the fact that promises chain.

Promises can chain with `.then` and you can return promises directly. Your code in `getStuffDone` can be rewritten as:

    function getStuffDone(param){
        return myPromiseFn(param+1); // much nicer, right?
    }
    

Promises are all about making asynchronous code more readable and behave like synchronous code without hiding that fact. Promises represent an abstraction over a value of one time operation, they abstract the notion of a statement or expression in a programming language.

You should only use deferred objects when you are [converting an API to promises](https://stackoverflow.com/questions/22519784/how-do-i-convert-an-existing-callback-api-to-promises) and can't do it automatically, or when you're writing aggregation functions that are easier expressed this way.

Quoting Esailija:

> This is the most common anti-pattern. It is easy to fall into this when you don't really understand promises and think of them as glorified event emitters or callback utility. Let's recap: promises are about making asynchronous code retain most of the lost properties of synchronous code such as flat indentation and one exception channel.
