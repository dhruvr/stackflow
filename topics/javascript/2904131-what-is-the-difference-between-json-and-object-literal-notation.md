
# What is the difference between JSON and Object Literal Notation?

## Question
        
Can someone tell me what is the main difference between a JavaScript object defined by using _"Object Literal Notation"_ and _JSON object_?

According to a JavaScript book it says this is an object defined by using _Object Notation_:

    var anObject = {
        property1 : true,
        showMessage : function (msg) { alert(msg) }
    };
    

Why isn't it a JSON object in this case? Just because it is not defined by using quotation marks?

## Answer
        
Lets clarify first what [_JSON_](http://www.json.org/) actually is. JSON is a _textual_, language-indepedent data-exchange format, much like XML, CSV or YAML.

Data can be stored in many ways, but if it should be stored in a text file and be readable by a computer, it needs to follow some structure. JSON is one of the many formats that define such a structure.

Such formats are typically language-independent, meaning they can be processed by Java, Python, JavaScript, PHP, you name it.

In contrast, _JavaScript_ is a programming language. Of course JavaScript also provides a way to define/describe data, but the syntax is very specific to JavaScript.

As a counter example, Python has the concept of _tuples_, their syntax is `(x, y)`. JavaScript doesn't have something like this.

* * *

Lets look at the syntactical differences between JSON and JavaScript object literals.

JSON has the following syntactical constraints:

*   Object _keys_ must be **strings** (i.e. a character sequence enclosed in double quotes `"`).
*   The values can be either:
    *   a string
    *   a number
    *   an (JSON) object
    *   an array
    *   `true`
    *   `false`
    *   `null`
*   Duplicate keys (`{"foo":"bar","foo":"baz"}`) produce undefined, implementation-specific results; the JSON specification specifically does not define their semantics

In JavaScript, object literals can have

*   String literals, number literals or identifier names as keys (since ES6, keys can now also be computed, which introduces yet another syntax).
*   The values can be any valid JavaScript expression, including function definitions and `undefined`.
*   Duplicate keys produce defined, specified results (in loose mode, the latter definition replaces the former; in strict mode, it's an error).

* * *

Knowing that, just be looking at the _syntax_, your example is not JSON because of two reasons:

1.  Your keys are not strings (literals). They are _identifier names_.
2.  You cannot assign a function as a value to a "JSON object" (because JSON doesn't define any syntax for functions).

But most importantly, to repeat my explanation from the beginning: You are in a JavaScript context. You define a JavaScript object. If any, a "JSON object" can only be contained in a string:

     var obj = {foo: 42}; // creates a JavaScript object (this is *not* JSON)
     var json = '{"foo": 452}'; // creates a string containing JSON
    

That is, if you're writing JavaScript source code, and not dealing with a _string_, you're not dealing with JSON. Maybe you received the data as JSON (e.g., via ajax or reading from a file), but once you or a library you're using has parsed it, it's not JSON anymore.

* * *

**Only because object literals and JSON look _similar_, it does not mean that you can name them interchangeably.** See also [There's no such thing as a "JSON Object"](http://benalman.com/news/2010/03/theres-no-such-thing-as-a-json/).
