
# Storing Objects in HTML5 localStorage

## Question
        
I'd like to store a JavaScript object in HTML5 `localStorage`, but my object is apparently being converted to a string.

I can store and retrieve primitive JavaScript types and arrays using `localStorage`, but objects don't seem to work. Should they?

Here's my code:

    var testObject = { 'one': 1, 'two': 2, 'three': 3 };
    console.log('typeof testObject: ' + typeof testObject);
    console.log('testObject properties:');
    for (var prop in testObject) {
        console.log('  ' + prop + ': ' + testObject[prop]);
    }
    
    // Put the object into storage
    localStorage.setItem('testObject', testObject);
    
    // Retrieve the object from storage
    var retrievedObject = localStorage.getItem('testObject');
    
    console.log('typeof retrievedObject: ' + typeof retrievedObject);
    console.log('Value of retrievedObject: ' + retrievedObject);
    

The console output is

    typeof testObject: object
    testObject properties:
      one: 1
      two: 2
      three: 3
    typeof retrievedObject: string
    Value of retrievedObject: [object Object]
    

It looks to me like the `setItem` method is converting the input to a string before storing it.

I see this behavior in Safari, Chrome, and Firefox, so I assume it's my misunderstanding of the [HTML5 Web Storage](http://www.w3.org/TR/webstorage/) spec, not a browser-specific bug or limitation.

I've tried to make sense of the _structured clone_ algorithm described in [http://www.w3.org/TR/html5/infrastructure.html](http://www.w3.org/TR/html5/infrastructure.html). I don't fully understand what it's saying, but maybe my problem has to do with my object's properties not being enumerable (???)

Is there an easy workaround?

* * *

Update: The W3C eventually changed their minds about the structured-clone specification, and decided to change the spec to match the implementations. See [https://www.w3.org/Bugs/Public/show_bug.cgi?id=12111](https://www.w3.org/Bugs/Public/show_bug.cgi?id=12111). So this question is no longer 100% valid, but the answers still may be of interest.

## Answer
        
Looking at the [Apple](http://developer.apple.com/safari/library/documentation/iPhone/Conceptual/SafariJSDatabaseGuide/Name-ValueStorage/Name-ValueStorage.html#//apple_ref/doc/uid/TP40007256-CH6-SW1), [Mozilla](https://developer.mozilla.org/en/DOM/Storage) and [Microsoft](http://msdn.microsoft.com/en-us/library/cc197050(VS.85).aspx) documentation, the functionality seems to be limited to handle only string key/value pairs.

A workaround can be to [_stringify_](http://www.json.org/js.html) your object before storing it, and later parse it when you retrieve it:

    var testObject = { 'one': 1, 'two': 2, 'three': 3 };
    
    // Put the object into storage
    localStorage.setItem('testObject', JSON.stringify(testObject));
    
    // Retrieve the object from storage
    var retrievedObject = localStorage.getItem('testObject');
    
    console.log('retrievedObject: ', JSON.parse(retrievedObject));
