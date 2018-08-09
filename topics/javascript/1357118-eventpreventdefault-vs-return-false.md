
# event.preventDefault() vs. return false

## Question
        
When I want to prevent other event handlers from executing after a certain event is fired, I can use one of two techniques. I'll use jQuery in the examples, but this applies to plain-JS as well:

### 1\. `event.preventDefault()`

    $('a').click(function (e) {
        // custom handling here
        e.preventDefault();
    });
    

### 2\. `return false`

    $('a').click(function () {
        // custom handling here
        return false;
    });
    

Is there any significant difference between those two methods of stopping event propagation?

For me, `return false;` is simpler, shorter and probably less error prone than executing a method. With the method, you have to remember about correct casing, parenthesis, etc.

Also, I have to define the first parameter in callback to be able to call the method. Perhaps, there are some reasons why I should avoid doing it like this and use `preventDefault` instead? What's the better way?

## Answer
        
`return false` from _within a jQuery event handler_ is effectively the same as calling both `e.preventDefault` and `e.stopPropagation` on the passed [jQuery.Event object.](http://api.jquery.com/category/events/event-object/)

`e.preventDefault()` will prevent the default event from occuring, `e.stopPropagation()` will prevent the event from bubbling up and `return false` will do both. Note that this behaviour differs from _normal_ (non-jQuery) event handlers, in which, notably, `return false` does _not_ stop the event from bubbling up.

Source: [John Resig](http://ejohn.org/)

[**Any benefit to using event.preventDefault() over "return false" to cancel out an href click?**](http://www.mail-archive.com/jquery-en@googlegroups.com/msg71371.html)
