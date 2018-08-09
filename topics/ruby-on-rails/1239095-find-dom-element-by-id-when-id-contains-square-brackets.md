
# Find DOM element by ID when ID contains square brackets?

## Question
        
I have a DOM element with an ID similar to:

    something[500]
    

which was built by my Ruby on Rails application. I need to be able to get this element via jQuery so that I can traverse my way up the DOM to delete the parent of it's parent, which has a variable ID that I don't have access to beforehand.

Does anyone know how I could go about this? The following code doesn't seem to be working:

    alert($("#something["+id+"]").parent().parent().attr("id"));
    

Upon further inspection, the following:

    $("#something["+id+"]")
    

returns an object, but when I run ".html()" or ".text()" on it, the result is always null or just an empty string.

Any help would be greatly appreciated.

## Answer
        
You need to escape the square brackets so that they are not counted as attribute selectors. Try this:

    alert($("#something\\["+id+"\\]").parent().parent().attr("id"));
    

[See Special Characters In Selectors](http://docs.jquery.com/Selectors), specifically the second paragraph:

> To use any of the meta-characters (such as `!"#$%&'()*+,./:;<=>?@[\]^``{|}~`) as a literal part of a name, it must be escaped with with two backslashes: `\\`. For example, an element with `id="foo.bar"`, can use the selector `$("#foo\\.bar")`. The W3C CSS specification contains the [complete set of rules regarding valid CSS selectors](https://www.w3.org/TR/CSS21/syndata.html#value-def-identifier). Also useful is the blog entry by Mathias Bynens on [CSS character escape sequences for identifiers](https://mathiasbynens.be/notes/css-escapes).
