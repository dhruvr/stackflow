
# How can I get query string values in JavaScript?

## Question
        
Is there a plugin-less way of retrieving [query string](http://en.wikipedia.org/wiki/Query_string) values via jQuery (or without)?

If so, how? If not, is there a plugin which can do so?

## Answer
        
You don't need jQuery for that purpose. You can use just some pure JavaScript:

    function getParameterByName(name, url) {
        if (!url) url = window.location.href;
        name = name.replace(/[\[\]]/g, '\\$&');
        var regex = new RegExp('[?&]' + name + '(=([^&#]*)|&|#|$)'),
            results = regex.exec(url);
        if (!results) return null;
        if (!results[2]) return '';
        return decodeURIComponent(results[2].replace(/\+/g, ' '));
    }
    

**Usage:**

    // query string: ?foo=lorem&bar=&baz
    var foo = getParameterByName('foo'); // "lorem"
    var bar = getParameterByName('bar'); // "" (present with empty value)
    var baz = getParameterByName('baz'); // "" (present with no value)
    var qux = getParameterByName('qux'); // null (absent)
    

  
Note: If a parameter is present several times (`?foo=lorem&foo=ipsum`), you will get the first value (`lorem`). There is no standard about this and usages vary, see for example this question: [Authoritative position of duplicate HTTP GET query keys](https://stackoverflow.com/questions/1746507/authoritative-position-of-duplicate-http-get-query-keys).  
NOTE: The function is case-sensitive. If you prefer case-insensitive parameter name, [add 'i' modifier to RegExp](https://stackoverflow.com/questions/3939715/case-insensitive-regex-in-javascript)

* * *

This is an update based on the new [URLSearchParams specs](https://developer.mozilla.org/en-US/docs/Web/API/URLSearchParams) to achieve the same result more succinctly. See answer titled "[URLSearchParams](https://stackoverflow.com/questions/901115/how-can-i-get-query-string-values-in-javascript/901144#12151322)" below.
