
# Event binding on dynamically created elements?

## Question
        
I have a bit of code where I am looping through all the select boxes on a page and binding a `.hover` event to them to do a bit of twiddling with their width on `mouse on/off`.

This happens on page ready and works just fine.

The problem I have is that any select boxes I add via Ajax or DOM after the initial loop won't have the event bound.

I have found this plugin ([jQuery Live Query Plugin](http://brandonaaron.net/docs/livequery/#getting-started)), but before I add another 5k to my pages with a plugin, I want to see if anyone knows a way to do this, either with jQuery directly or by another option.

## Answer
        
**As of jQuery 1.7** you should use [`jQuery.fn.on`](https://api.jquery.com/on/#on-events-selector-data-handler):

    $(staticAncestors).on(eventName, dynamicChild, function() {});
    

* * *

**Prior to this**, the recommended approach was to use [`live()`](http://api.jquery.com/live):

    $(selector).live( eventName, function(){} );
    

However, `live()` was deprecated in 1.7 in favour of `on()`, and completely removed in 1.9. The `live()` signature:

    $(selector).live( eventName, function(){} );
    

... can be replaced with the following [`on()`](http://api.jquery.com/on/) signature:

    $(document).on( eventName, selector, function(){} );
    

* * *

For example, if your page was dynamically creating elements with the class name `dosomething` you would bind the event to **a parent which already exists** (this is the nub of the problem here, you need something that exists to bind to, don't bind to the dynamic content), this can be (and the easiest option) is `document`. Though bear in mind [`document` may not be the most efficient option](https://stackoverflow.com/questions/12824549/should-all-jquery-events-be-bound-to-document).

    $(document).on('mouseover mouseout', '.dosomething', function(){
        // what you want to happen when mouseover and mouseout 
        // occurs on elements that match '.dosomething'
    });
    

Any parent that exists at the time the event is bound is fine. For example

    $('.buttons').on('click', 'button', function(){
        // do something here
    });
    

would apply to

    <div class="buttons">
        <!-- <button>s that are generated dynamically and added here -->
    </div>
