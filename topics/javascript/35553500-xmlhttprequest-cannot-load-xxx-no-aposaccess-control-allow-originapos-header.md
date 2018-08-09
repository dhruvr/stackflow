
# XMLHttpRequest cannot load XXX No &apos;Access-Control-Allow-Origin&apos; header

## Question
        
tl;dr; About the Same Origin Policy
-----------------------------------

I have a Grunt process which initiates an instance of express.js server. This was working absolutely fine up until just now when it started serving a blank page with the following appearing in the error log in the developer's console in Chrome (latest version):

> XMLHttpRequest cannot load [https://www.example.com/](https://www.example.com/) No 'Access-Control-Allow-Origin' header is present on the requested resource. Origin '[http://localhost:4300](http://localhost:4300)' is therefore not allowed access.

What is stopping me from accessing the page?

## Answer
        
About the Same Origin Policy
============================

This is the [Same Origin Policy](https://developer.mozilla.org/en-US/docs/Web/Security/Same-origin_policy). It is a security feature implemented by browsers.

Your particular case is showing how it is implemented for XMLHttpRequest (and you'll get identical results if you were to use fetch), but it also applies to other things (such as images loaded onto a `<canvas>` or documents loaded into an `<iframe>`), just with slightly different implementations.

The standard scenario that demonstrates the need for the SOP can be demonstrated with [three characters](https://en.wikipedia.org/wiki/Alice_and_Bob):

*   Alice is a person with a web browser
*   Bob runs a website (`https://www.[website].com/` in your example)
*   Mallory runs a website (`http://localhost:4300` in your example)

Alice is logged into Bob's site and has some confidential data there. Perhaps it is a company intranet (accessible only to browsers on the LAN), or her online banking (accessible only with a cookie you get after entering a username and password).

Alice visits Mallory's website which has some JavaScript that causes Alice's browser to make an HTTP request to Bob's website (from her IP address with her cookies, etc). This could be as simple as using `XMLHttpRequest` and reading the `responseText`.

The browser's Same Origin Policy prevents that JavaScript from reading the data returned by Bob's website (which Bob and Alice don't want Mallory to access). (Note that you can, for example, display an image using an `<img>` element across origins because the content of the image is not exposed to JavaScript (or Mallory) … unless you throw canvas into the mix in which case you _will_ generate a same-origin violation error).

* * *

Why the Same Origin Policy applies when you don't think it should
=================================================================

For any given URL it is possible that the SOP is not needed. A couple of common scenarios where this is the case are:

*   Alice, Bob and Mallory are the same person.
*   Bob is providing entirely public information

… but the browser has no way of knowing if either of the above are true, so trust is not automatic and the SOP is applied. Permission has to be granted explicitly before the browser will give the data it was given to a different website.

* * *

Why the Same Origin Policy only applies to JavaScript in a web page
===================================================================

Browser extensions, the Network tab in browser developer tools and applications like Postman are installed software. They aren't passing data from one website to the JavaScript belonging to a different website _just because you visited that different website_. Installing software usually takes a more conscious choice.

There isn't a third party (Mallory) who is considered a risk.

* * *

Why you can display data in the page without reading it with JS
===============================================================

There are a number of circumstances where Mallory's site can cause a browser to fetch data from a third party and display it (e.g. by adding an `<img>` element to display an image). It isn't possible for Mallory's JavaScript to read the data in that resource though, only Alice's browser and Bob's server can do that, so it is still secure.

* * *

CORS
====

The `Access-Control-Allow-Origin` header referred to in the error message is part of the [CORS](https://developer.mozilla.org/en-US/docs/Web/HTTP/Access_control_CORS) standard which allows Bob to explicitly grant permission to Mallory's site to access the data via Alice's browser.

A basic implementation would just include:

    Access-Control-Allow-Origin: *
    

… to permit any website to read the data.

    Access-Control-Allow-Origin: http://example.com/
    

… would allow only a specific site to access it, and you can dynamically generate that based on the `Origin` _request_ header to permit multiple, but not all, sites to access it.

The specifics of how you set that response header depend on Bob's HTTP server and/or server-side programming language. There is [a collection of guides for various common configurations](https://enable-cors.org/server.html) that might help.

NB: Some requests are complex and send a [preflight](https://developer.mozilla.org/en-US/docs/Web/HTTP/Access_control_CORS#Preflighted_requests) OPTIONS request that the server will have to respond to before the browser will send the GET/POST/PUT/Whatever request that the JS wants to make. Implementations of CORS that only add `Access-Control-Allow-Origin` to specific URLs often get tripped up by this.

* * *

Obviously granting permission via CORS is something Bob would only do only if either:

*   The data was not private _or_
*   Mallory was trusted

If you are also Bob in this scenario, then the specifics of how you add CORS permission headers will depend on some combination of your choice of HTTP server software and what language you are using for server-side programming (if any).

_Mallory_ can't add this header because she has to get permission from Bob's site and it would be silly (to the point of rendering the SOP useless) for her to be able to grant herself permission.

* * *

Error messages which mention "Response for preflight"
=====================================================

Some cross origin requests are [preflighted](https://developer.mozilla.org/en-US/docs/Web/HTTP/Access_control_CORS#Preflighted_requests).

This happens when (roughly speaking) you try to make a cross-origin request that:

*   Includes credentials like cookies
*   Couldn't be generated with a regular HTML form (e.g. has custom headers or a Content-Type that you couldn't use in a form's `enctype`).

Note that "custom headers" include `Access-Control-Allow-Origin` and other CORS response headers. These don't belong on the request, don't do anything helpful (what would be the point of a permissions system where you could grant yourself permission?), and must appear only on the response.

In these cases then **the rest of this answer still applies** but you also need to make sure that the server can listen for the preflight request (which will be `OPTIONS` (and not `GET`, `POST` or whatever you were trying to send) and respond to it with the right `Access-Control-Allow-Origin` header but also `Access-Control-Allow-Methods` and `Access-Control-Allow-Headers` to allow your specific HTTP methods or headers.

* * *

Alternatives to CORS
====================

JSONP
-----

Bob could also provide the data using a hack like [JSONP](https://stackoverflow.com/questions/2067472/what-is-jsonp-all-about) which is how people did cross-origin Ajax before CORS came along.

It works by presenting the data in the form of a JavaScript program which injects the data into Mallory's page.

It requires that Mallory trust Bob not to provide malicious code.

Note the common theme: The site providing the data has to tell the browser that it is OK for a third party site to access the data it is sending to the browser.

Move the two resources to a single Origin
-----------------------------------------

If the HTML document the JS runs in and the URL being requested are on the same origin (sharing the same scheme, hostname, and port) then they Same Origin Policy grants permission by default. CORS is not needed.

A Proxy
-------

Mallory _could_ use server-side code to fetch the data (which she could then pass from her server to Alice's browser through HTTP as usual).

It will either:

*   add CORS headers
*   convert the response to JSONP
*   exist on the same origin as the HTML document

That server-side code could be hosted by a third party (such as YQL).

Bob wouldn't need to grant any permissions for that to happen.

This would be fine since that is just between Mallory and Bob. There is no way for Bob to think that Mallory is Alice and to provide Mallory with data that should be kept confidential between Alice and Bob.

Consequently, Mallory can only use this technique to read _public_ data.

Writing something other than a web app
--------------------------------------

As noted in the section "Why the Same Origin Policy only applies to JavaScript in a web page", you can avoid the SOP by not writing JavaScript in a webpage.

That doesn't mean you can't continue to use JavaScript and HTML, but you could distribute it using some other mechanism, such as Node-WebKit or PhoneGap.

Browser extensions
==================

It is possible for a browser extension to inject the CORS headers in the response before the Same Origin Policy is applied.

These can be useful for development, but are not practical for a production site (asking every user of your site to install a browser extension that disables a security feature of their browser is unreasonable).

They also tend to work only with simple requests (failing when handling preflight OPTIONS requests).

Having a proper development environment with a local development _server_ is usually a better approach.

* * *

Other security risks
====================

Note that SOP / CORS do not mitigate [XSS](https://en.wikipedia.org/wiki/Cross-site_scripting), [CSRF](https://en.wikipedia.org/wiki/Cross-site_request_forgery), or [SQL Injection](https://en.wikipedia.org/wiki/SQL_injection) attacks which need to be handled independently.

* * *

Summary
=======

*   There is nothing you can do in _your_ client-side code that will enable CORS access to someone _else's_ server.
*   If you control the server the request is being made to: Add CORS permissions to it.
*   If you are friendly with the person who controls it: Get them to add CORS permissions to it.
*   If it is a public service: Read their API documentation to see what they say about accessing it with client-side JavaScript. They might tell you to use specific URLs or to use JSONP (or they might not support it at all).
*   If none of the above apply: Get the browser to talk to _your_ server instead, and then have your server fetch the data from the other server and pass it on. (There are also third-party hosted services which attach CORS headers to publically accessible resources that you could use).
