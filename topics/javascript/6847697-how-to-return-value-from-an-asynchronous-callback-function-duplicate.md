
# How to return value from an asynchronous callback function? [duplicate]

## Question
        
This question already has an answer here:

*   [How do I return the response from an asynchronous call?](/questions/14220321/how-do-i-return-the-response-from-an-asynchronous-call) 32 answers

This question is asked many times in SO. But still I can't get stuff.

I want to get some value from callback. Look at the script below for clarification.

    function foo(address){
    
          // google map stuff
          geocoder.geocode( { 'address': address}, function(results, status) {
              results[0].geometry.location; // I want to return this value
          })
    
        }
        foo(); //result should be results[0].geometry.location; value
    

If I try to return that value just getting "undefined". I followed some ideas from SO, but still fails.

Those are:

    function foo(address){
        var returnvalue;    
        geocoder.geocode( { 'address': address}, function(results, status) {
            returnvalue = results[0].geometry.location; 
        })
        return returnvalue; 
    }
    foo(); //still undefined

## Answer
        
This is impossible as you cannot return from an asynchronous call inside a synchronous method.

In this case you need to pass a callback to foo that will receive the return value

    function foo(address, fn){
      geocoder.geocode( { 'address': address}, function(results, status) {
         fn(results[0].geometry.location); 
      });
    }
    
    foo("address", function(location){
      alert(location); // this is where you get the return value
    });
    

The thing is, if an inner function call is asynchronous, then all the functions 'wrapping' this call must also be asynchronous in order to 'return' a response.

If you have a lot of callbacks you might consider taking the plunge and use a [promise library like Q](https://github.com/kriskowal/q).
