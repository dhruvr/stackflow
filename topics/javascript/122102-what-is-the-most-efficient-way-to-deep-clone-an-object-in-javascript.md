
# What is the most efficient way to deep clone an object in JavaScript?

## Question
        
What is the most efficient way to clone a JavaScript object? I've seen `obj = eval(uneval(o));` being used, but [that's non-standard and only supported by Firefox](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/uneval).  
  
I've done things like `obj = JSON.parse(JSON.stringify(o));` but question the efficiency.  
  
I've also seen recursive copying functions with various flaws.  
I'm surprised no canonical solution exists.

## Answer
        
> **Note:** This is a reply to another answer, not a proper response to this question. If you wish to have fast object cloning please follow [Corban's advice in their answer](https://stackoverflow.com/a/5344074/1438393) to this question.

* * *

I want to note that the [`.clone()`](http://api.jquery.com/clone/) method in **jQuery** only clones DOM elements. In order to clone JavaScript objects, you would do:

    // Shallow copy
    var newObject = jQuery.extend({}, oldObject);
    
    // Deep copy
    var newObject = jQuery.extend(true, {}, oldObject);
    

More information can be found in the [jQuery documentation](http://api.jquery.com/jQuery.extend/).

I also want to note that the deep copy is actually much smarter than what is shown above – it's able to avoid many traps (trying to deep extend a DOM element, for example). It's used frequently in jQuery core and in plugins to great effect.
