
# Ways to circumvent the same-origin policy

## Question
        
The same origin policy
----------------------

I wanted to make a community wiki regarding HTML/JS **same-origin policies** to hopefully help anyone searching for this topic. This is one of the most searched-for topics on SO and there is no consolidated wiki for it so here I go :)

> The same origin policy prevents a document or script loaded from one origin from getting or setting properties of a document from another origin. This policy dates all the way back to Netscape Navigator 2.0.

What are some of your favorite ways to go around same-origin policies?
----------------------------------------------------------------------

Please keep examples verbose and preferably also link your sources.

## Answer
        
The `document.domain` method
----------------------------

*   Method type: **iframe**.

Note that this is an iframe method that sets the value of document.domain to a suffix of the current domain. If it does so, the shorter domain is used for subsequent origin checks. For example, assume a script in the document at `http://store.company.com/dir/other.html` executes the following statement:

    document.domain = "company.com";
    

After that statement executes, the page would pass the origin check with `http://company.com/dir/page.html`. However, by the same reasoning, company.com could not set `document.domain` to `othercompany.com`.

With this method, you would be allowed to exectue javascript from an iframe sourced on a subdomain on a page sourced on the main domain. This method is not suited for cross-domain resources as browsers like Firefox will not allow you to change the `document.domain` to a completely alien domain.

Source: [https://developer.mozilla.org/en/Same\_origin\_policy\_for\_JavaScript](https://developer.mozilla.org/en/Same_origin_policy_for_JavaScript)

The Cross-Origin Resource Sharing method
----------------------------------------

*   Method type: **AJAX**.

[Cross-Origin Resource Sharing](http://www.w3.org/TR/access-control/) (CORS) is a W3C Working Draft that defines how the browser and server must communicate when accessing sources across origins. The basic idea behind CORS is to use custom HTTP headers to allow both the browser and the server to know enough about each other to determine if the request or response should succeed or fail.

For a simple request, one that uses either `GET` or `POST` with no custom headers and whose body is `text/plain`, the request is sent with an extra header called `Origin`. The Origin header contains the origin (protocol, domain name, and port) of the requesting page so that the server can easily determine whether or not it should serve a response. An example `Origin` header might look like this:

    Origin: http://www.stackoverflow.com
    

If the server decides that the request should be allowed, it sends a `Access-Control-Allow-Origin` header echoing back the same origin that was sent or `*` if it’s a public resource. For example:

    Access-Control-Allow-Origin: http://www.stackoverflow.com
    

If this header is missing, or the origins don’t match, then the browser disallows the request. If all is well, then the browser processes the request. Note that neither the requests nor responses include cookie information.

The Mozilla team suggests in [their post about CORS](http://hacks.mozilla.org/2009/07/cross-site-xmlhttprequest-with-cors/) that you should check for the existence of the `withCredentials` property to determine if the browser supports CORS via XHR. You can then couple with the existence of the `XDomainRequest` object to cover all browsers:

    function createCORSRequest(method, url){
        var xhr = new XMLHttpRequest();
        if ("withCredentials" in xhr){
            xhr.open(method, url, true);
        } else if (typeof XDomainRequest != "undefined"){
            xhr = new XDomainRequest();
            xhr.open(method, url);
        } else {
            xhr = null;
        }
        return xhr;
    }
    
    var request = createCORSRequest("get", "http://www.stackoverflow.com/");
    if (request){
        request.onload = function() {
            // ...
        };
        request.onreadystatechange = handler;
        request.send();
    }
    

Note that for the CORS method to work, you need to have access to any type of server header mechanic and can't simply access any third-party resource.

Source: [http://www.nczonline.net/blog/2010/05/25/cross-domain-ajax-with-cross-origin-resource-sharing/](http://www.nczonline.net/blog/2010/05/25/cross-domain-ajax-with-cross-origin-resource-sharing/)

The `window.postMessage` method
-------------------------------

*   Method type: **iframe**.

`window.postMessage`, when called, causes a `MessageEvent` to be dispatched at the target window when any pending script that must be executed completes (e.g. remaining event handlers if `window.postMessage` is called from an event handler, previously-set pending timeouts, etc.). The `MessageEvent` has the type message, a `data` property which is set to the string value of the first argument provided to `window.postMessage`, an `origin` property corresponding to the origin of the main document in the window calling `window.postMessage` at the time `window.postMessage` was called, and a `source` property which is the window from which `window.postMessage` is called.

To use `window.postMessage`, an event listener must be attached:

        // Internet Explorer
        window.attachEvent('onmessage',receiveMessage);
    
        // Opera/Mozilla/Webkit
        window.addEventListener("message", receiveMessage, false);
    

And a `receiveMessage` function must be declared:

    function receiveMessage(event)
    {
        // do something with event.data;
    }
    

The off-site iframe must also send events properly via `postMessage`:

    <script>window.parent.postMessage('foo','*')</script>
    

Any window may access this method on any other window, at any time, regardless of the location of the document in the window, to send it a message. Consequently, any event listener used to receive messages must first check the identity of the sender of the message, using the origin and possibly source properties. This cannot be understated: **Failure to check the `origin` and possibly `source` properties enables cross-site scripting attacks.**

Source: [https://developer.mozilla.org/en/DOM/window.postMessage](https://developer.mozilla.org/en/DOM/window.postMessage)
