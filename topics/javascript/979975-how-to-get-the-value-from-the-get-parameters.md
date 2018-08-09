
# How to get the value from the GET parameters?

## Question
        
I have a URL with some GET parameters as follows:

    www.test.com/t.html?a=1&b=3&c=m2-m3-m4-m5 
    

I need to get the whole value of `c`. I tried to read the URL, but I got only `m2`. How do I do this using JavaScript?

## Answer
        
JavaScript itself has nothing built in for handling query string parameters.

In a (modern) browser you can use the [`URL` object](https://developer.mozilla.org/en-US/docs/Web/API/URLSearchParams);

    var url_string = "http://www.example.com/t.html?a=1&b=3&c=m2-m3-m4-m5"; //window.location.href
    var url = new URL(url_string);
    var c = url.searchParams.get("c");
    console.log(c);

* * *

For older browsers (including Internet Explorer), you can use [this polyfill](https://github.com/github/url-polyfill) or the code from the original version of this answer that predates `URL`:

You could access `location.search`, which would give you from the `?` character on to the end of the URL or the start of the fragment identifier (#foo), whichever comes first.

Then you can parse it with this:

    function parse_query_string(query) {
      var vars = query.split("&");
      var query_string = {};
      for (var i = 0; i < vars.length; i++) {
        var pair = vars[i].split("=");
        var key = decodeURIComponent(pair[0]);
        var value = decodeURIComponent(pair[1]);
        // If first entry with this name
        if (typeof query_string[key] === "undefined") {
          query_string[key] = decodeURIComponent(value);
          // If second entry with this name
        } else if (typeof query_string[key] === "string") {
          var arr = [query_string[key], decodeURIComponent(value)];
          query_string[key] = arr;
          // If third or later entry with this name
        } else {
          query_string[key].push(decodeURIComponent(value));
        }
      }
      return query_string;
    }
    
    var query_string = "a=1&b=3&c=m2-m3-m4-m5";
    var parsed_qs = parse_query_string(query_string);
    console.log(parsed_qs.c);

You can get the query string from the URL of the current page with:

    var query = window.location.search.substring(1);
    var qs = parse_query_string(query);
