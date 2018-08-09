
# Abort Ajax requests using jQuery

## Question
        
Using jQuery, how can I **cancel/abort an Ajax request** that I have not yet received the response from?

## Answer
        
Most of the jQuery Ajax methods return an XMLHttpRequest (or the equivalent) object, so you can just use `abort()`.

See the documentation:

*   [abort Method](http://msdn.microsoft.com/en-us/library/ms535920%28VS.85%29.aspx) ([MSDN](http://en.wikipedia.org/wiki/Microsoft_Developer_Network)). Cancels the current HTTP request.
*   [abort()](https://developer.mozilla.org/en-US/docs/Web/API/XMLHttpRequest/abort) ([MDN](http://en.wikipedia.org/wiki/Mozilla_Developer_Network)). If the request has been sent already, this method will abort the request.

    var xhr = $.ajax({
        type: "POST",
        url: "some.php",
        data: "name=John&location=Boston",
        success: function(msg){
           alert( "Data Saved: " + msg );
        }
    });
    
    //kill the request
    xhr.abort()
    

**UPDATE:** As of jQuery 1.5 the returned object is a wrapper for the native XMLHttpRequest object called jqXHR. This object appears to expose all of the native properties and methods so the above example still works. See _[The jqXHR Object](http://api.jquery.com/jQuery.ajax/#jqXHR)_ (jQuery API documentation).

**UPDATE 2:** As of jQuery 3, the ajax method now returns a promise with extra methods (like abort), so the above code still works, though the object being returned is not an `xhr` any more. See the [3.0 blog here](http://blog.jquery.com/2016/01/14/jquery-3-0-beta-released/).

**UPDATE 3**: `xhr.abort()` still works on jQuery 3.x. Don't assume the _update 2_ is correct. [More info on jQuery Github repository](https://github.com/jquery/jquery/issues/2084#issuecomment-308173243).
