
# Change an element&apos;s class with JavaScript

## Question
        
How can I change a class of an HTML element in response to an `onClick` event using JavaScript?

## Answer
        
Modern HTML5 Techniques for changing classes
--------------------------------------------

Modern browsers have added [**classList**](https://developer.mozilla.org/en-US/docs/DOM/element.classList) which provides methods to make it easier to manipulate classes without needing a library:

    document.getElementById("MyElement").classList.add('MyClass');
    
    document.getElementById("MyElement").classList.remove('MyClass');
    
    if ( document.getElementById("MyElement").classList.contains('MyClass') )
    
    document.getElementById("MyElement").classList.toggle('MyClass');
    

Unfortunately, these do not work in Internet Explorer prior to v10, though there is a [shim](http://en.wikipedia.org/wiki/Shim_(computing)) to add support for it to IE8 and IE9, available from [this page](https://developer.mozilla.org/en-US/docs/DOM/element.classList). It is, though, getting more and more [supported](http://caniuse.com/#feat=classlist).

Simple cross-browser solution
-----------------------------

The standard JavaScript way to select an element is using [`document.getElementById("Id")`](https://developer.mozilla.org/en-US/docs/DOM/document.getElementById), which is what the following examples use - you can of course obtain elements in other ways, and in the right situation may simply use `this` instead - however, going into detail on this is beyond the scope of the answer.

### To change all classes for an element:

To replace all existing classes with one or more new classes, set the className attribute:

    document.getElementById("MyElement").className = "MyClass";
    

(You can use a space-delimited list to apply multiple classes.)

### To add an additional class to an element:

To add a class to an element, without removing/affecting existing values, append a space and the new classname, like so:

    document.getElementById("MyElement").className += " MyClass";
    

### To remove a class from an element:

To remove a single class to an element, without affecting other potential classes, a simple regex replace is required:

    document.getElementById("MyElement").className =
       document.getElementById("MyElement").className.replace
          ( /(?:^|\s)MyClass(?!\S)/g , '' )
    /* Code wrapped for readability - above is all one statement */
    

An explanation of this regex is as follows:

    (?:^|\s) # Match the start of the string, or any single whitespace character
    
    MyClass  # The literal text for the classname to remove
    
    (?!\S)   # Negative lookahead to verify the above is the whole classname
             # Ensures there is no non-space character following
             # (i.e. must be end of string or a space)
    

The `g` flag tells the replace to repeat as required, in case the class name has been added multiple times.

### To check if a class is already applied to an element:

The same regex used above for removing a class can also be used as a check as to whether a particular class exists:

    if ( document.getElementById("MyElement").className.match(/(?:^|\s)MyClass(?!\S)/) )
    

  

### Assigning these actions to onclick events:

Whilst it is possible to write JavaScript directly inside the HTML event attributes (such as `onclick="this.className+=' MyClass'"`) this is not recommended behaviour. Especially on larger applications, more maintainable code is achieved by separating HTML markup from JavaScript interaction logic.

The first step to achieving this is by creating a function, and calling the function in the onclick attribute, for example:

    <script type="text/javascript">
        function changeClass(){
            // Code examples from above
        }
    </script>
    ...
    <button onclick="changeClass()">My Button</button>
    

_(It is not required to have this code in script tags, this is simply for brevity of example, and including the JavaScript in a distinct file may be more appropriate.)_

The second step is to move the onclick event out of the HTML and into JavaScript, for example using [addEventListener](https://developer.mozilla.org/en-US/docs/DOM/element.addEventListener)

    <script type="text/javascript">
        function changeClass(){
            // Code examples from above
        }
    
        window.onload = function(){
            document.getElementById("MyElement").addEventListener( 'click', changeClass);
        }
    </script>
    ...
    <button id="MyElement">My Button</button>
    

(Note that the window.onload part is required so that the contents of that function are executed _after_ the HTML has finished loading - without this, the MyElement might not exist when the JavaScript code is called, so that line would fail.)

  

JavaScript Frameworks and Libraries
-----------------------------------

The above code is all in standard JavaScript, however it is common practise to use either a framework or a library to simplify common tasks, as well as benefit from fixed bugs and edge cases that you might not think of when writing your code.

Whilst some people consider it overkill to add a ~50 KB framework for simply changing a class, if you are doing any substantial amount of JavaScript work, or anything that might have unusual cross-browser behaviour, it is well worth considering.

_(Very roughly, a library is a set of tools designed for a specific task, whilst a framework generally contains multiple libraries and performs a complete set of duties.)_

The examples above have been reproduced below using [jQuery](http://jquery.com), probably the most commonly used JavaScript library (though there are others worth investigating too).

(Note that `$` here is the jQuery object.)

### Changing Classes with jQuery:

    $('#MyElement').addClass('MyClass');
    
    $('#MyElement').removeClass('MyClass');
    
    if ( $('#MyElement').hasClass('MyClass') )
    

In addition, jQuery provides a shortcut for adding a class if it doesn't apply, or removing a class that does:

    $('#MyElement').toggleClass('MyClass');
    

  

### Assigning a function to a click event with jQuery:

    $('#MyElement').click(changeClass);
    

or, without needing an id:

    $(':button:contains(My Button)').click(changeClass);
