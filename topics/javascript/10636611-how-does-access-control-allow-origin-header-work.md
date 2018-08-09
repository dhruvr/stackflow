
# How does Access-Control-Allow-Origin header work?

## Question
        
Apparently, I have completely misunderstood its semantics. I thought of something like this:

1.  A client downloads javascript code MyCode.js from http://siteA - **the origin**.
2.  The response header of MyCode.js contains **Access-Control-Allow-Origin: http://siteB**, which I thought meant that MyCode.js was allowed to make cross-origin references to the site B.
3.  The client triggers some functionality of MyCode.js, which in turn make requests to http://siteB, which should be fine, despite being cross-origin requests.

Well, I am wrong. It does not work like this at all. So, I have read [Cross-origin resource sharing](http://en.wikipedia.org/wiki/Cross-origin_resource_sharing) and attempted to read [Cross-Origin Resource Sharing in w3c recommendation](http://www.w3.org/TR/cors/.)

One thing is sure - I still do not understand how am I supposed to use this header.

I have full control of both site A and site B. How do I enable the javascript code downloaded from the site A to access resources on the site B using this header?

P.S.

I do not want to utilize JSONP.

## Answer
        
`Access-Control-Allow-Origin` is a [CORS (Cross-Origin Resource Sharing) header](http://www.html5rocks.com/en/tutorials/cors/).

When Site A tries to fetch content from Site B, Site B can send an `Access-Control-Allow-Origin` response header to tell the browser that the content of this page is accessible to certain origins. (An _origin_ is a [domain, plus a scheme and port number](https://stackoverflow.com/a/19542686/710446).) By default, Site B's pages are [not accessible to any other origin](https://en.wikipedia.org/wiki/Same-origin_policy); using the `Access-Control-Allow-Origin` header opens a door for cross-origin access by specific requesting origins.

For each resource/page that Site B wants to make accessible to Site A, Site B should serve its pages with the response header:

    Access-Control-Allow-Origin: http://siteA.com
    

Modern browsers will not block cross-domain requests outright. If Site A requests a page from Site B, the browser will actually fetch the requested page _on the network level_ and check if the response headers list Site A as a permitted requester domain. If Site B has not indicated that Site A is allowed to access this page, the browser will trigger the `XMLHttpRequest`'s `error` event and deny the response data to the requesting JavaScript code.

Non-simple requests
===================

What happens on the network level can be _slightly_ more complex than explained above. If the request is a ["non-simple" request](http://www.html5rocks.com/en/tutorials/cors/#toc-handling-a-not-so-simple-request), the browser first sends a data-less "preflight" OPTIONS request, to verify that the server will accept the request. A request is non-simple when either (or both):

*   using an HTTP verb other than GET or POST (e.g. PUT, DELETE)
*   using non-simple request headers; the only simple requests headers are:
    *   `Accept`
    *   `Accept-Language`
    *   `Content-Language`
    *   `Content-Type` (this is only simple when its value is `application/x-www-form-urlencoded`, `multipart/form-data`, or `text/plain`)

If the server responds to the OPTIONS preflight with appropriate response headers (`Access-Control-Allow-Headers` for non-simple headers, `Access-Control-Allow-Methods` for non-simple verbs) that match the non-simple verb and/or non-simple headers, then the browser sends the actual request.

Supposing that Site A wants to send a PUT request for `/somePage`, with a non-simple `Content-Type` value of `application/json`, the browser would first send a preflight request:

    OPTIONS /somePage HTTP/1.1
    Origin: http://siteA.com
    Access-Control-Request-Method: PUT
    Access-Control-Request-Headers: Content-Type
    

Note that `Access-Control-Request-Method` and `Access-Control-Request-Headers` are added by the browser automatically; you do not need to add them. This OPTIONS preflight gets the successful response headers:

    Access-Control-Allow-Origin: http://siteA.com
    Access-Control-Allow-Methods: GET, POST, PUT
    Access-Control-Allow-Headers: Content-Type
    

When sending the actual request (after preflight is done), the behavior is identical to how a simple request is handled. In other words, a non-simple request whose preflight is successful is treated the same as a simple request (i.e., the server must still send `Access-Control-Allow-Origin` again for the actual response).

The browsers sends the actual request:

    PUT /somePage HTTP/1.1
    Origin: http://siteA.com
    Content-Type: application/json
    
    { "myRequestContent": "JSON is so great" }
    

And the server sends back an `Access-Control-Allow-Origin`, just as it would for a simple request:

    Access-Control-Allow-Origin: http://siteA.com
    

See [Understanding XMLHttpRequest over CORS](https://stackoverflow.com/a/13400954/710446) for a little more information about non-simple requests.
