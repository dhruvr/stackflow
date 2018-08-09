
# Creating multiline strings in JavaScript

## Question
        
I have the following code in Ruby. I want to convert this code into JavaScript. what's the equivalent code in JS?

    text = <<"HERE"
    This
    Is
    A
    Multiline
    String
    HERE

## Answer
        
### Update:

ECMAScript 6 (ES6) introduces a new type of literal, namely [**template literals**](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/template_strings). They have many features, variable interpolation among others, but most importantly for this question, they can be multiline.

A template literal is delimited by _backticks_:

    var html = `
      <div>
        <span>Some HTML here</span>
      </div>
    `;
    

(Note: I'm not advocating to use HTML in strings)

[Browser support is OK](https://kangax.github.io/compat-table/es6/#test-template_literals), but you can use [transpilers](https://babeljs.io/) to be more compatible.

* * *

### Original ES5 answer:

Javascript doesn't have a here-document syntax. You can escape the literal newline, however, which comes close:

    "foo \
    bar"
