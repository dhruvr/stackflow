
# What is JSONP all about?

## Question
        
I understand JSON, but not JSONP. [Wikipedia's document on JSON](http://en.wikipedia.org/wiki/JSON) is (was) the top search result for JSONP. It says this:

> JSONP or "JSON with padding" is a JSON extension wherein a prefix is specified as an input argument of the call itself.

Huh? What call? That doesn't make any sense to me. JSON is a data format. There's no call.

The [2nd search result](http://remysharp.com/2007/10/08/what-is-jsonp/) is from some guy named [Remy](https://stackoverflow.com/users/22617/remy-sharp), who writes this about JSONP:

> JSONP is script tag injection, passing the response from the server in to a user specified function.

I can sort of understand that, but it's still not making any sense.

* * *

So what is JSONP? Why was it created (what problem does it solve)? And why would I use it?

* * *

**Addendum**: I've just created [a new page for JSONP](http://en.wikipedia.org/wiki/JSONP) on Wikipedia; it now has a clear and thorough description of JSONP, based on [jvenema](https://stackoverflow.com/users/25330/jvenema)'s answer.

## Answer
        
It's actually not too complicated...

Say you're on domain example.com, and you want to make a request to domain example.net. To do so, you need to cross domain boundaries, a no-no in most of browserland.

The one item that bypasses this limitation is <script> tags. When you use a script tag, the domain limitation is ignored, but under normal circumstances, you can't really **do** anything with the results, the script just gets evaluated.

Enter JSONP. When you make your request to a server that is JSONP enabled, you pass a special parameter that tells the server a little bit about your page. That way, the server is able to nicely wrap up its response in a way that your page can handle.

For example, say the server expects a parameter called "callback" to enable its JSONP capabilities. Then your request would look like:

    http://www.example.net/sample.aspx?callback=mycallback
    

Without JSONP, this might return some basic JavaScript object, like so:

    { foo: 'bar' }
    

However, with JSONP, when the server receives the "callback" parameter, it wraps up the result a little differently, returning something like this:

    mycallback({ foo: 'bar' });
    

As you can see, it will now invoke the method you specified. So, in your page, you define the callback function:

    mycallback = function(data){
      alert(data.foo);
    };
    

And now, when the script is loaded, it'll be evaluated, and your function will be executed. Voila, cross-domain requests!

It's also worth noting the one major issue with JSONP: you lose a lot of control of the request. For example, there is no "nice" way to get proper failure codes back. As a result, you end up using timers to monitor the request, etc, which is always a bit suspect. The proposition for [JSONRequest](http://www.json.org/JSONRequest.html) is a great solution to allowing cross domain scripting, maintaining security, and allowing proper control of the request.

These days (2015), [CORS](http://en.wikipedia.org/wiki/Cross-origin_resource_sharing) is the recommended approach vs. JSONRequest. JSONP is still useful for older browser support, but given the security implications, unless you have no choice CORS is the better choice.
