
# Insert code into the page context using a content script

## Question
        
I'm learning how to create Chrome extensions. I just started developing one to catch YouTube events. I want to use it with YouTube flash player (later I will try to make it compatible with HTML5).

**manifest.json:**

    {
        "name": "MyExtension",
        "version": "1.0",
        "description": "Gotta catch Youtube events!",
        "permissions": ["tabs", "http://*/*"],
        "content_scripts" : [{
            "matches" : [ "www.youtube.com/*"],
            "js" : ["myScript.js"]
        }]
    }
    

**myScript.js:**

    function state() { console.log("State Changed!"); }
    var player = document.getElementById("movie_player");
    player.addEventListener("onStateChange", "state");
    console.log("Started!");
    

The problem is that the console gives me the _"Started!"_, but there is no _"State Changed!"_ when I play/pause YouTube videos.

When this code is put in the console, it worked. What am I doing wrong?

## Answer
        
Content scripts are executed in an ["isolated world" environment](https://developer.chrome.com/extensions/content_scripts#execution-environment). You have to inject your `state()` method into the page itself.

When you want to use one of the `chrome.*` APIs in the script, you have to implement a special event handler, as described in this answer: [Chrome extension - retrieving Gmail's original message](https://stackoverflow.com/a/9636008/938089?chrome-extension-retrieving-gmails-original-message).

Otherwise, if you don't have to use `chrome.*` APIs, I strongly recommend to inject all of your JS code in the page via adding a `<script>` tag:

Table of contents
=================

*   Method 1: Inject another file
*   Method 2: Inject embedded code
*   Method 2b: Using a function
*   Method 3: Using an inline event
*   Dynamic values in the injected code

Method 1: Inject another file
-----------------------------

_This is the easiest/best method when you have lots of code._ Include your actual JS code in a file within your extension, say `script.js`. Then let your content script be as follows (explained here: [Google Chome “Application Shortcut” Custom Javascript](https://stackoverflow.com/a/9310273/938089?google-chome-application-shortcut-custom-javascript)):

    var s = document.createElement('script');
    // TODO: add "script.js" to web_accessible_resources in manifest.json
    s.src = chrome.extension.getURL('script.js');
    s.onload = function() {
        this.remove();
    };
    (document.head || document.documentElement).appendChild(s);
    

**Note: If you use this method, the injected `script.js` file has to be added to the [`"web_accessible_resources"`](https://developer.chrome.com/extensions/manifest/web_accessible_resources.html) section** ([example](https://stackoverflow.com/a/10529675/938089?google-chrome-extension-script-injections)). If you do not, Chrome will **refuse** to load your script and display the following error in the console:

> Denying load of chrome-extension://\[EXTENSIONID\]/script.js. Resources must be listed in the web\_accessible\_resources manifest key in order to be loaded by pages outside the extension.

Method 2: Inject embedded code
------------------------------

This method is useful when you want to quickly run a small piece of code. (See also: [How to disable facebook hotkeys with Chrome extension?](https://stackoverflow.com/a/8994454/938089?how-to-disable-facebook-hotkeys-with-chrome-extension)).

    var actualCode = `// Code here.
    // If you want to use a variable, use $ and curly braces.
    // For example, to use a fixed random number:
    var someFixedRandomValue = ${ Math.random() };
    // NOTE: Do not insert unsafe variables in this way, see below
    // at "Dynamic values in the injected code"
    `;
    
    var script = document.createElement('script');
    script.textContent = actualCode;
    (document.head||document.documentElement).appendChild(script);
    script.remove();
    

Note: [template literals](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Template_literals) are only supported in Chrome 41 and above. If you want the extension to work in Chrome 40-, use:

    var actualCode = ['/* Code here. Example: */' + 'alert(0);',
                      '// Beware! This array have to be joined',
                      '// using a newline. Otherwise, missing semicolons',
                      '// or single-line comments (//) will mess up your',
                      '// code ----->'].join('\n');
    

Method 2b: Using a function
---------------------------

For a big chunk of code, quoting the string is not feasible. Instead of using an array, a function can be used, and stringified:

    var actualCode = '(' + function() {
        // All code is executed in a local scope.
        // For example, the following does NOT overwrite the global `alert` method
        var alert = null;
        // To overwrite a global variable, prefix `window`:
        window.alert = null;
    } + ')();';
    var script = document.createElement('script');
    script.textContent = actualCode;
    (document.head||document.documentElement).appendChild(script);
    script.remove();
    

This method works, because the `+` operator on strings and a function converts all objects to a string. If you intend on using the code more than once, it's wise to create a function to avoid code repetition. An implementation might look like:

    function injectScript(func) {
        var actualCode = '(' + func + ')();'
        ...
    }
    injectScript(function() {
       alert("Injected script");
    });
    

Note: Since the function is serialized, the original scope, and all bound properties are lost!

    var scriptToInject = function() {
        console.log(typeof scriptToInject);
    };
    injectScript(scriptToInject);
    // Console output:  "undefined"
    

Method 3: Using an inline event
-------------------------------

Sometimes, you want to run some code immediately, e.g. to run some code before the `<head>` element is created. This can be done by inserting a `<script>` tag with `textContent` (see method 2/2b).

An alternative, **but not recommended** is to use inline events. It is not recommended because if the page defines a Content Security policy that forbids inline scripts, then inline event listeners are blocked. Inline scripts injected by the extension, on the other hand, still run. If you still want to use inline events, this is how:

    var actualCode = '// Some code example \n' + 
                     'console.log(document.documentElement.outerHTML);';
    
    document.documentElement.setAttribute('onreset', actualCode);
    document.documentElement.dispatchEvent(new CustomEvent('reset'));
    document.documentElement.removeAttribute('onreset');
    

Note: This method assumes that there are no other global event listeners that handle the `reset` event. If there is, you can also pick one of the other global events. Just open the JavaScript console (F12), type `document.documentElement.on`, and pick on of the available events.

Dynamic values in the injected code
-----------------------------------

Occasionally, you need to pass an arbitrary variable to the injected function. For example:

    var GREETING = "Hi, I'm ";
    var NAME = "Rob";
    var scriptToInject = function() {
        alert(GREETING + NAME);
    };
    

To inject this code, you need to pass the variables as arguments to the anonymous function. Be sure to implement it correctly! The following will **not** work:

    var scriptToInject = function (GREETING, NAME) { ... };
    var actualCode = '(' + scriptToInject + ')(' + GREETING + ',' + NAME ')';
    // The previous will work for numbers and booleans, but not strings.
    // To see why, have a look at the resulting string:
    var actualCode = "(function(GREETING, NAME) {...})(Hi I'm,Rob)";
    //                                                 ^^^^^^ ^^^ No string literals!
    

The solution is to use [`JSON.stringify`](https://developer.mozilla.org/en-US/docs/JavaScript/Reference/Global_Objects/JSON/stringify) before passing the argument. Example:

    var actualCode = '(' + function(greeting, name) { ...
    } + ')(' + JSON.stringify(GREETING) + ',' + JSON.stringify(NAME) + ')';
    

If you have many variables, it's worthwhile to use `JSON.stringify` once, to improve readability, as follows:

    ...
    } + ')(' + JSON.stringify([arg1, arg2, arg3, arg4]) + ')';
