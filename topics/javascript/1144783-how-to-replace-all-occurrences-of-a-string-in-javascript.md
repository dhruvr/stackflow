
# How to replace all occurrences of a string in JavaScript?

## Question
        
I have this string:

    "Test abc test test abc test test test abc test test abc"
    

Doing

    str = str.replace('abc', '');
    

seems to only remove the first occurrence of `abc` in the string above. How can I replace **all** occurrences of it?

## Answer
        
For the sake of completeness, I got to thinking about which method I should use to do this. There are basically two ways to do this as suggested by the other answers on this page.

**Note:** In general, extending the built-in prototypes in JavaScript is generally not recommended. I am providing as extensions on the String prototype simply for purposes of illustration, showing different implementations of a hypothetical standard method on the `String` built-in prototype.

* * *

### Regular Expression Based Implementation

    String.prototype.replaceAll = function(search, replacement) {
        var target = this;
        return target.replace(new RegExp(search, 'g'), replacement);
    };
    

### Split and Join (Functional) Implementation

    String.prototype.replaceAll = function(search, replacement) {
        var target = this;
        return target.split(search).join(replacement);
    };
    

* * *

Not knowing too much about how regular expressions work behind the scenes in terms of efficiency, I tended to lean toward the split and join implementation in the past without thinking about performance. When I did wonder which was more efficient, and by what margin, I used it as an excuse to find out.

On my Chrome Windows 8 machine, **the regular expression based implementation is the fastest**, with the **split and join implementation being 53% slower**. Meaning the regular expressions are twice as fast for the lorem ipsum input I used.

Check out this [**benchmark**](http://jsben.ch/#/LFfWA) running these two implementations against each other.

* * *

As noted in the comment below by @ThomasLeduc and others, there could be an issue with the regular expression-based implementation if `search` contains certain characters which are reserved as [special characters in regular expressions](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions#Using_special_characters). The implementation assumes that the caller will escape the string beforehand or will only pass strings that are without the characters in the table in _[Regular Expressions](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions#Using_special_characters)_ (MDN).

MDN also provides an implementation to escape our strings. It would be nice if this was also standardized as `RegExp.escape(str)`, but alas, it does not exist:

    function escapeRegExp(str) {
      return str.replace(/[.*+?^${}()|[\]\\]/g, "\\$&"); // $& means the whole matched string
    }
    

We could call `escapeRegExp` within our `String.prototype.replaceAll` implementation, however, I'm not sure how much this will affect the performance (potentially even for strings for which the escape is not needed, like all alphanumeric strings).
