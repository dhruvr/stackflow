
# How do I include a JavaScript file in another JavaScript file?

## Question
        
Is there something in JavaScript similar to `@import` in CSS that allows you to include a JavaScript file inside another JavaScript file?

## Answer
        
The old versions of JavaScript had no import, include, or require, so many different approaches to this problem have been developed.

But recent versions of JavaScript have standards like [ES6 modules](http://exploringjs.com/es6/ch_modules.html) to import modules, although this is not supported yet by most browsers. Many people using modules with browser applications use [build](https://webpack.github.io/) and/or [transpilation](https://babeljs.io/) tools to make it practical to use new syntax with features like modules.

### ES6 Modules

Note that currently, browser support for ES6 Modules is not particularly great, but it is on its way. According to [this StackOverflow answer](https://stackoverflow.com/a/44086319), they are supported in Chrome 61, Firefox 54(behind the `dom.moduleScripts.enabled` setting in `about:config`) and MS Edge 16, with only Safari 10.1 providing support without flags.

Thus, you will currently still need to use build and/or transpilation tools to valid JavaScript that will run in without any requirement for the user to use those browser versions or enable any flags.

Once ES6 Modules are commonplace, here is how you would go about using them:

    // module.js
    export function hello() {
      return "Hello";
    }
    

    // main.js
    import {hello} from 'module'; // or './module'
    let val = hello(); // val is "Hello";
    

### Node.js require

Node.js is currently using a [module.exports/require](https://nodejs.org/api/modules.html) system. You can use `babel` to transpile if you want the `import` syntax.

    // mymodule.js
    module.exports = {
       hello: function() {
          return "Hello";
       }
    }
    

    // server.js
    const myModule = require('./mymodule');
    let val = myModule.hello(); // val is "Hello"   
    

There are other ways for JavaScript to include external JavaScript contents in browsers that do not require preprocessing.

### AJAX Loading

You could load an additional script with an AJAX call and then use `eval` to run it. This is the most straightforward way, but it is limited to your domain because of the JavaScript sandbox security model. Using `eval` also opens the door to bugs, hacks and security issues.

### jQuery Loading

The [jQuery](http://jquery.com/) library provides loading functionality [in one line](http://api.jquery.com/jQuery.getScript/):

    $.getScript("my_lovely_script.js", function() {
       alert("Script loaded but not necessarily executed.");
    });
    

### Dynamic Script Loading

You could add a script tag with the script URL into the HTML. To avoid the overhead of jQuery, this is an ideal solution.

The script can even reside on a different server. Furthermore, the browser evaluates the code. The `<script>` tag can be injected into either the web page `<head>`, or inserted just before the closing `</body>` tag.

Here is an example of how this could work:

    function dynamicallyLoadScript(url) {
        var script = document.createElement("script"); // Make a script DOM node
        script.src = url; // Set it's src to the provided URL
    
        document.head.appendChild(script); // Add it to the end of the head section of the page (could change 'head' to 'body' to add it to the end of the body section instead)
    }
    

This function will add a new `<script>` tag to end of the head section of the page, where the `src` attribute is set to the URL which is given to the function as the first parameter.

Both of these solutions are discussed and illustrated in [JavaScript Madness: Dynamic Script Loading](http://unixpapa.com/js/dyna.html).

Detecting when the script has been executed
-------------------------------------------

Now, there is a big issue you must know about. Doing that implies that _you remotely load the code_. Modern web browsers will load the file and keep executing your current script because they load everything asynchronously to improve performance. (This applies to both the jQuery method and the manual dynamic script loading method.)

It means that if you use these tricks directly, _you won't be able to use your newly loaded code the next line after you asked it to be loaded_, because it will be still loading.

For example: `my_lovely_script.js` contains `MySuperObject`:

    var js = document.createElement("script");
    
    js.type = "text/javascript";
    js.src = jsFilePath;
    
    document.body.appendChild(js);
    
    var s = new MySuperObject();
    
    Error : MySuperObject is undefined
    

Then you reload the page hitting F5. And it works! Confusing...

**So what to do about it ?**

Well, you can use the hack the author suggests in the link I gave you. In summary, for people in a hurry, he uses an event to run a callback function when the script is loaded. So you can put all the code using the remote library in the callback function. For example:

    function loadScript(url, callback)
    {
        // Adding the script tag to the head as suggested before
        var head = document.getElementsByTagName('head')[0];
        var script = document.createElement('script');
        script.type = 'text/javascript';
        script.src = url;
    
        // Then bind the event to the callback function.
        // There are several events for cross browser compatibility.
        script.onreadystatechange = callback;
        script.onload = callback;
    
        // Fire the loading
        head.appendChild(script);
    }
    

Then you write the code you want to use AFTER the script is loaded in a [lambda function](http://en.wikipedia.org/wiki/Anonymous_function):

    var myPrettyCode = function() {
       // Here, do whatever you want
    };
    

Then you run all that:

    loadScript("my_lovely_script.js", myPrettyCode);
    

Note that the script may execute after the DOM has loaded, or before, depending on the browser and whether you included the line `script.async = false;`. There's a [great article on Javascript loading in general](http://www.html5rocks.com/en/tutorials/speed/script-loading/) which discusses this.

### Source Code Merge/Preprocessing

As mentioned at the top of this answer, many developers now use build/transpilation tool(s) like WebPack, Babel, or Gulp in their projects, allowing them to use new syntax and support modules better, combine files, minify, etc.
