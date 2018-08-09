
# Why does jQuery or a DOM method such as getElementById not find the element?

## Question
        
What are the possible reasons for `document.getElementById`, `$("#id")` or any other DOM method / jQuery selector not finding the elements?

Example problems include:

jQuery silently failing to bind an event handler, and a standard DOM method returning `null` resulting in the error:

> Uncaught TypeError: Cannot set property '...' of null

## Answer
        
The element you were trying to find wasn’t in the [DOM](https://developer.mozilla.org/en-US/docs/Web/API/Document_Object_Model) when your script ran.

The position of your DOM-reliant script can have a profound effect upon its behavior. Browsers parse HTML documents from top to bottom. Elements are added to the DOM and scripts are (generally) executed as they're encountered. **This means that order matters.** Typically, scripts can't find elements which appear later in the markup because those elements have yet to be added to the DOM.

Consider the following markup; script #1 fails to find the `<div>` while script #2 succeeds:

    <script>
      console.log("script #1: %o", document.getElementById("test")); // null
    </script>
    <div id="test">test div</div>
    <script>
      console.log("script #2: %o", document.getElementById("test")); // <div id="test" ...
    </script>

So, what should you do? You've got a few options:

* * *

Option 1: Move your script
==========================

Move your script further down the page, just before the closing body tag. Organized in this fashion, the rest of the document is parsed before your script is executed:

    <body>
      <button id="test">click me</button>
      <script>
        document.getElementById("test").addEventListener("click", function() {
          console.log("clicked: %o", this);
        });
      </script>
    </body><!-- closing body tag -->

Note: Placing scripts at the bottom is generally considered a [best practice](http://developer.yahoo.com/performance/rules.html#js_bottom).

* * *

Option 2: jQuery's `ready()`
============================

Defer your script until the DOM has been completely parsed, using [`ready()`](http://api.jquery.com/ready/):

    <script src="https://ajax.googleapis.com/ajax/libs/jquery/2.1.1/jquery.min.js"></script>
    <script>
      $(document).ready(function() {
        $("#test").click(function() {
          console.log("clicked: %o", this);
        });
      });
    </script>
    <button id="test">click me</button>

Note: You could simply bind to [`DOMContentLoaded`](https://developer.mozilla.org/en-US/docs/Web/Reference/Events/DOMContentLoaded) or `window.[onload](https://developer.mozilla.org/en-US/docs/Web/API/window.onload?redirect=no)` but each has its caveats. jQuery's [`ready()`](http://api.jquery.com/ready/) delivers a hybrid solution.

* * *

Option 3: Event Delegation
==========================

> [Delegated events](http://api.jquery.com/on/#direct-and-delegated-events) have the advantage that they can process events from descendant elements that are added to the document at a later time.

When an element raises an event (provided that it's a [bubbling](http://www.w3.org/TR/DOM-Level-2-Events/events.html#Events-flow-bubbling) event and nothing stops its propagation), each parent in that element's ancestry receives the event as well. That allows us to attach a handler to an existing element and sample events as they bubble up from its descendants... even those added after the handler is attached. All we have to do is check the event to see whether it was raised by the desired element and, if so, run our code.

jQuery's [`on()`](http://api.jquery.com/on/) performs that logic for us. We simply provide an event name, a selector for the desired descendant, and an event handler:

    <script src="https://ajax.googleapis.com/ajax/libs/jquery/2.1.1/jquery.min.js"></script>
    <script>
      $(document).on("click", "#test", function(e) {
        console.log("clicked: %o",  this);
      });
    </script>
    <button id="test">click me</button>

Note: Typically, this pattern is reserved for elements which didn't exist at load-time _or_ to avoid attaching a large amount of handlers. It's also worth pointing out that while I've attached a handler to `document` (for demonstrative purposes), you should select the nearest reliable ancestor.

* * *

Option 4: The `defer` attribute
===============================

Use the [`defer`](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/script#attr-defer) attribute of `<script>`.

> \[`defer`, a Boolean attribute,\] is set to indicate to a browser that the script is meant to be executed after the document has been parsed.

    <script src="https://gh-canon.github.io/misc-demos/log-test-click.js" defer></script>
    <button id="test">click me</button>

For reference, here's the code from that [external script](https://gh-canon.github.io/misc-demos/log-test-click.js):

    document.getElementById("test").addEventListener("click", function(e){
       console.log("clicked: %o", this); 
    });
    

Note: The `defer` attribute certainly _seems_ like a magic bullet _but_ it's important to be aware of the caveats...  
1\. `defer` can only be used for external scripts, i.e.: those having a `src` attribute.  
2\. be aware of [browser support](http://caniuse.com/script-defer), i.e.: buggy implementation in IE < 10
