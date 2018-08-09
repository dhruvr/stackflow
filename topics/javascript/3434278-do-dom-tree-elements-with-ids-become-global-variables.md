
# Do DOM tree elements with ids become global variables?

## Question
        
Working on an idea for a simple HTMLElement wrapper I stumbled upon the following for Internet Explorer and [Chrome](http://en.wikipedia.org/wiki/Google_Chrome):

For a given HTMLElement with ID in the DOM tree, it is possible to retrieve the div using its ID as the variable name. So for a div like

    <div id="example">some text</div>
    

in [Internet Explorer 8](http://en.wikipedia.org/wiki/Internet_Explorer_8) and Chrome you can do:

    alert(example.innerHTML); //=> 'some text'
    

or

    alert(window['example'].innerHTML); //=> 'some text'
    

So, does this mean _every element in the DOM tree_ is converted to a variable in the global namespace? And does it also mean one can use this as a replacement for the `getElementById` method in these browsers?

## Answer
        
What is supposed to happen is that ‘named elements’ are added as apparent properties of the `document` object. This is a really bad idea, as it allows element names to clash with real properties of `document`.

IE made the situation worse by also adding named elements as properties of the `window` object. This is doubly bad in that now you have to avoid naming your elements after any member of either the `document` or the `window` object you (or any other library code in your project) might want to use.

It also means that these elements are visible as global-like variables. Luckily in this case any real global `var` or `function` declarations in your code shadow them, so you don't need to worry so much about naming here, but if you try to do an assignment to a global variable with a clashing name and you forget to declare it `var`, you'll get an error in IE as it tries to assign the value to the element itself.

It's generally considered bad practice to omit `var`, as well as to rely on named elements being visible on `window` or as globals. Stick to `document.getElementById`, which is more widely-supported and less ambiguous. You can write a trivial wrapper function with a shorter name if you don't like the typing. Either way, there's no point in using an id-to-element lookup cache, because browsers typically optimise the `getElementById` call to use a quick lookup anyway; all you get is problems when elements change `id` or are added/removed from the document.

Opera copied IE, then WebKit joined in, and now both the previously-unstandardised practice of putting named elements on `document` properties, and the previously-IE-only practice of putting them on `window` are [being](https://www.w3.org/TR/html5/dom.html#dom-tree-accessors) [standardised](https://www.w3.org/TR/html5/browsers.html#named-access-on-the-window-object) by HTML5, whose approach is to document and standardise every terrible practice inflicted on us by browser authors, making them part of the web forever. So Firefox 4 will also support this.

What are ‘named elements’? Anything with an `id`, and anything with a `name` being used for ‘identifying’ purposes: that is, forms, images, anchors and a few others, but not other unrelated instances of a `name` attribute, like control-names in form input fields, parameter names in `<param>` or metadata type in `<meta>`. ‘Identifying’ `name`s are the ones that should should be avoided in favour of `id`.
