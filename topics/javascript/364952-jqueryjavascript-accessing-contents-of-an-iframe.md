
# jQuery/JavaScript: accessing contents of an iframe

## Question
        
I would like to manipulate the HTML inside an iframe using jQuery.

I thought I'd be able to do this by setting the context of the jQuery function to be the document of the iframe, something like:

    $(function(){ //document ready
        $('some selector', frames['nameOfMyIframe'].document).doStuff()
    });
    

However this doesn't seem to work. A bit of inspection shows me that the variables in `frames['nameOfMyIframe']` are `undefined` unless I wait a while for the iframe to load. However, when the iframe loads the variables are not accessible (I get `permission denied`-type errors).

Does anyone know of a work-around to this?

## Answer
        
I think what you are doing is subject to the [same origin policy](http://en.wikipedia.org/wiki/Same_origin_policy). This should be the reason why you are getting _permission denied type_ errors.
