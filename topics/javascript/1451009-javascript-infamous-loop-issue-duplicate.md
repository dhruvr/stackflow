
# Javascript infamous Loop issue? [duplicate]

## Question
        
This question already has an answer here:

*   [JavaScript closure inside loops – simple practical example](/questions/750486/javascript-closure-inside-loops-simple-practical-example) 38 answers

I've got the following code snippet.

    function addLinks () {
        for (var i=0, link; i<5; i++) {
            link = document.createElement("a");
            link.innerHTML = "Link " + i;
            link.onclick = function () {
                alert(i);
            };
            document.body.appendChild(link);
        }
    }
    

The above code is for generating 5 links and bind each link with an alert event to show the current link id. But It doesn't work. When you click the generated links they all say "link 5".

But the following codes snippet works as our expectation.

    function addLinks () {
        for (var i=0, link; i<5; i++) {
            link = document.createElement("a");
            link.innerHTML = "Link " + i;
            link.onclick = function (num) {
                return function () {
                    alert(num);
                };
            }(i);
            document.body.appendChild(link);
        }
    }
    

The above 2 snippets are quoted from [here](http://robertnyman.com/2008/10/09/explaining-javascript-scope-and-closures/ "here"). As the author's explanation, seems the **closure** makes the magic.

But how it works and How **closure** makes it work are all beyond my understanding. Why the first one doesn't work while the second one works? Can anyone give a detailed explanation about the magic?

thanks.

## Answer
        
[Quoting myself](https://stackoverflow.com/questions/643542/doesnt-javascript-support-closures-with-local-variables/643664#643664) for an explanation of the first example:

> JavaScript's scopes are function-level, not block-level, and creating a closure just means that the enclosing scope gets added to the lexical environment of the enclosed function.
> 
> After the loop terminates, the function-level variable i has the value 5, and that's what the inner function 'sees'.

In the second example, for each iteration step the outer function literal will evaluate to a new function object with its own scope and local variable `num`, whose value is set to the current value of `i`. As `num` is never modified, it will stay constant over the lifetime of the closure: The next iteration step doesn't overwrite the old value as the function objects are independant.

Keep in mind that this approach is rather inefficient as two new function objects have to be created for each link. This is unnecessary, as they can easily be shared if you use the DOM node for information storage:

    function linkListener() {
        alert(this.i);
    }
    
    function addLinks () {
        for(var i = 0; i < 5; ++i) {
            var link = document.createElement('a');
            link.appendChild(document.createTextNode('Link ' + i));
            link.i = i;
            link.onclick = linkListener;
            document.body.appendChild(link);
        }
    }
