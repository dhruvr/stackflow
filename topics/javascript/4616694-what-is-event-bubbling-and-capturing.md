
# What is event bubbling and capturing?

## Question
        
What is the difference between event bubbling and capturing? Of the two, which is the faster and better model to use?

## Answer
        
Event bubbling and capturing are two ways of event propagation in the HTML DOM API, when an event occurs in an element inside another element, and both elements have registered a handle for that event. The event propagation mode determines in [which order the elements receive the event](http://www.quirksmode.org/js/events_order.html).

With bubbling, the event is first captured and handled by the innermost element and then propagated to outer elements.

With capturing, the event is first captured by the outermost element and propagated to the inner elements.

Capturing is also called "trickling", which helps remember the propagation order:

> trickle down, bubble up

Back in the old days, Netscape advocated event capturing, while Microsoft promoted event bubbling. Both are part of the W3C [Document Object Model Events](http://www.w3.org/TR/DOM-Level-2-Events/events.html) standard (2000).

IE < 9 uses [only event bubbling](https://developer.mozilla.org/en-US/docs/Web/API/EventTarget.addEventListener), whereas IE9+ and all major browsers support both. On the other hand, the [performance of event bubbling may be slightly lower](https://stackoverflow.com/a/10335117/1269037) for complex DOMs.

We can use the `addEventListener(type, listener, useCapture)` to register event handlers for in either bubbling (default) or capturing mode. To use the capturing model pass the third argument as `true`.

Example
-------

    <div>
        <ul>
            <li></li>
        </ul>
    </div>
    

In the structure above, assume that a click event occurred in the `li` element.

In capturing model, the event will be handled by the `div` first (click event handlers in the `div` will fire first), then in the `ul`, then at the last in the target element, `li`.

In the bubbling model, the opposite will happen: the event will be first handled by the `li`, then by the `ul`, and at last by the `div` element.

For more information, see

*   [Event Order](http://www.quirksmode.org/js/events_order.html) on QuirksMode
*   [addEventListener](https://developer.mozilla.org/en-US/docs/Web/API/EventTarget.addEventListener) on MDN
*   [Events Advanced](http://www.quirksmode.org/js/events_advanced.html) on QuirksMode

In the example below, if you click on any of the highlighted elements, you can see that the capturing phase of the event propagation flow occurs first, followed by the bubbling phase.

    var logElement = document.getElementById('log');
    
    function log(msg) {
        logElement.innerHTML += ('<p>' + msg + '</p>');
    }
    
    function capture() {
        log('capture: ' + this.firstChild.nodeValue.trim());
    }
    
    function bubble() {
        log('bubble: ' + this.firstChild.nodeValue.trim());
    }
    
    var divs = document.getElementsByTagName('div');
    for (var i = 0; i < divs.length; i++) {
        divs[i].addEventListener('click', capture, true);
        divs[i].addEventListener('click', bubble, false);
    }

    p {
        line-height: 0;
    }
    
    div {
        display:inline-block;
        padding: 5px;
    
        background: #fff;
        border: 1px solid #aaa;
        cursor: pointer;
    }
    
    div:hover {
        border: 1px solid #faa;
        background: #fdd;
    }

    <div>1
        <div>2
            <div>3
                <div>4
                    <div>5</div>
                </div>
            </div>
        </div>
    </div>
    <section id="log"></section>

[Another example at JSFiddle](http://jsfiddle.net/cwtuan/je1g3f29/16).
