
# Parse JSON in JavaScript? [duplicate]

## Question
        
This question already has an answer here:

*   [Safely turning a JSON string into an object](/questions/45015/safely-turning-a-json-string-into-an-object) 23 answers

I want to parse a JSON string in JavaScript. The response is something like

    var response = '{"result":true,"count":1}';
    

How can I get the values `result` and `count` from this?

## Answer
        
Most browsers support [`JSON.parse()`](http://msdn.microsoft.com/en-us/library/cc836466(v=vs.85).aspx), which is defined in ECMA-262 5th Edition (the specification that JavaScript is based on). Its usage is simple:

    var json = '{"result":true,"count":1}',
        obj = JSON.parse(json);
    
    alert(obj.count);
    

For the browsers that don't you can implement it using [json2.js](https://github.com/douglascrockford/JSON-js/blob/master/json2.js).

As noted in the comments, if you're already using jQuery, there is a `$.parseJSON` function that maps to `JSON.parse` if available or a form of `eval` in older browsers. However, this performs additional, unnecessary checks that are also performed by `JSON.parse`, so for the best all round performance I'd recommend using it like so:

    var json = '{"result":true,"count":1}',
        obj = JSON && JSON.parse(json) || $.parseJSON(json);
    

This will ensure you use native `JSON.parse` immediately, rather than having jQuery perform sanity checks on the string before passing it to the native parsing function.
