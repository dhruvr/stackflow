
# How do I convert an existing callback API to promises?

## Question
        
I want to work with promises but I have a callback API in a format like:

### 1\. DOM load or other one time event:

    window.onload; // set to callback
    ...
    window.onload = function(){
    
    };
    

### 2\. Plain callback:

    function request(onChangeHandler){
        ...
    }
    request(function(){
        // change happened
        ...
    });
    

### 3\. Node style callback ("nodeback"):

    function getStuff(dat,callback){
        ...
    }
    getStuff("dataParam",function(err,data){
        ...
    })
    

### 4\. A whole library with node style callbacks:

    API;
    API.one(function(err,data){
        API.two(function(err,data2){
            API.three(function(err,data3){
                ...
            });
        });
    });
    

### How do I work with the API in promises, how do I "promisify" it?

## Answer
        
Promises have state, they start as pending and can settle to:

*   **fulfilled** meaning that the computation completed successfully.
*   **rejected** meaning that the computation failed.

Promise returning functions _should never throw_, they should return rejections instead. Throwing from a promise returning function will force you to use both a `} catch {` _and_ a `.catch`. People using promisified APIs do not expect promises to throw. If you're not sure how async APIs work in JS - please [see this answer](https://stackoverflow.com/questions/14220321/how-to-return-the-response-from-an-asynchronous-call/16825593#16825593) first.

### 1\. DOM load or other one time event:

So, creating promises generally means specifying when they settle - that means when they move to the fulfilled or rejected phase to indicate the data is available (and can be accessed with `.then`).

With modern promise implementations that support the `Promise` constructor like native ES6 promises:

    function load(){
        return new Promise(function(resolve,reject){
             window.onload = resolve;
        });
    }
    

You would then use the resulting promise like so:

    load().then(function(){
        // Do things after onload
    });
    

With libraries that support deferred (Let's use $q for this example here, but we'll also use jQuery later):

    function load(){
        var d = $q.defer();
        window.onload = function(){ d.resolve(); };
        return d.promise;
    }
    

Or with a jQuery like API, hooking on an event happening once:

    function done(){
        var d = $.Deferred();
        $("#myObject").once("click",function(){
             d.resolve();
        });
        return d.promise();
    }
    

### 2\. Plain callback:

These APIs are rather common since well... callbacks are common in JS. Let's look at the common case of having `onSuccess` and `onFail`:

     function getUserData(userId, onLoad, onFail){ ...
    

With modern promise implementations that support the `Promise` constructor like native ES6 promises:

    function getUserDataAsync(userId){
        return new Promise(function(resolve,reject){
             getUserData(userId,resolve,reject);
        });
    }
    

With libraries that support deferred (Let's use jQuery for this example here, but we've also used $q above):

    function getUserDataAsync(userId){
        var d = $.Deferred();
        getUserData(userId,function(res){ d.resolve(res); } ,function(err){ d.reject(err); });
        return d.promise();
    }
    

jQuery also offers a `$.Deferred(fn)` form, which has the advantage of allowing us to write an expression that emulates very closely the `new Promise(fn)` form, as follows:

    function getUserDataAsync(userId) {
        return $.Deferred(function(dfrd) {
            getUserData(userId, dfrd.resolve, dfrd.reject);
        }).promise();
    }
    

Note: Here we exploit the fact that a jQuery deferred's `resolve` and `reject` methods are "detachable"; ie. they are bound to the _instance_ of a jQuery.Deferred(). Not all libs offer this feature.

### 3\. Node style callback ("nodeback"):

Node style callbacks (nodebacks) have a particular format where the callbacks is always the last argument and its first parameter is an error. Let's first promisify one manually:

    getStuff("dataParam",function(err,data){
    

To:

    function getStuffAsync(param){
        return new Promise(function(resolve,reject){
             getStuff(param,function(err,data){
                 if(err !== null) return reject(err);
                 resolve(data);
             });
        });
    }
    

With deferreds you can do the following (let's use Q for this example, although Q now supports the new syntax which you should prefer):

    function getStuffAsync(param){
        var d = Q.defer();
        getStuff(param,function(err,data){
             if(err !== null) return d.reject(err); // `throw err` also works here.
                 d.resolve(data);
        });
        return d.promise;   
    }
    

In general, you should not promisify things manually too much, most promise libraries that were designed with Node in mind as well as native promises in Node 8+ have a built in method for promisifying nodebacks. For example

    var getStuffAsync = Promise.promisify(getStuff); // Bluebird
    var getStuffAsync = Q.denodeify(getStuff); // Q
    var getStuffAsync = util.promisify(getStuff); // Native promises, node only
    

### 4\. A whole library with node style callbacks:

There is no golden rule here, you promisify them one by one. However, some promise implementations allow you to do this in bulk, for example in Bluebird, converting a nodeback API to a promise API is as simple as:

    Promise.promisifyAll(API);
    

Or with _native promises_ in **Node**:

    const { promisify } = require('util');
    const promiseAPI = Object.entries(API).map(v => ({key, fn: promisify(v)}))
                             .reduce((o, p) => Object.assign(o, {[p.key] : p.fn}), {});
    

Notes:

*   Of course, when you are in a `.then` handler you do not need to promisify things. Returning a promise from a `.then` handler will resolve or reject with that promise's value. Throwing from a `.then` handler is also good practice and will reject the promise - this is the famous promise throw safety.
*   In an actual `onload` case, you should use `addEventListener` rather than `onX`.
