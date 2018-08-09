
# Why does a RegExp with global flag give wrong results?

## Question
        
What is the problem with this regular expression when I use the global flag and the case insensitive flag? Query is a user generated input. The result should be \[true, true\].

    var query = 'Foo B';
    var re = new RegExp(query, 'gi');
    var result = [];
    result.push(re.test('Foo Bar'));
    result.push(re.test('Foo Bar'));
    // result will be [true, false]
    

* * *

    var reg = /^a$/g;
    for(i = 0; i++ < 10;)
       console.log(reg.test("a"));

## Answer
        
The `RegExp` object keeps track of the [`lastIndex`](https://developer.mozilla.org/en/Core_JavaScript_1.5_Reference/Global_Objects/RegExp/lastIndex) where a match occurred, so on subsequent matches it will start from the last used index, instead of 0. Take a look:

    var query = 'Foo B';
    var re = new RegExp(query, 'gi');
    var result = [];
    result.push(re.test('Foo Bar'));
    
    alert(re.lastIndex);
    
    result.push(re.test('Foo Bar'));
    

If you don't want to manually reset `lastIndex` to 0 after every test, just remove the `g` flag.

Here's the algorithm that the specs dictate (section 15.10.6.2):

> **RegExp.prototype.exec(string)**
> 
> Performs a regular expression match of string against the regular expression and returns an Array object containing the results of the match, or null if the string did not match The string ToString(string) is searched for an occurrence of the regular expression pattern as follows:
> 
> 1.  Let S be the value of ToString(string).
> 2.  Let length be the length of S.
> 3.  **Let lastIndex be the value of the lastIndex property.**
> 4.  Let i be the value of ToInteger(lastIndex).
> 5.  If the global property is false, let i = 0.
> 6.  If I < 0 or I > length then set lastIndex to 0 and return null.
> 7.  Call \[\[Match\]\], giving it the arguments S and i. If \[\[Match\]\] returned failure, go to step 8; otherwise let r be its State result and go to step 10.
> 8.  Let i = i+1.
> 9.  Go to step 6.
> 10.  Let e be r's endIndex value.
> 11.  **If the global property is true, set lastIndex to e.**
> 12.  Let n be the length of r's captures array. (This is the same value as 15.10.2.1's NCapturingParens.)
> 13.  Return a new array with the following properties:
>     *   The index property is set to the position of the matched substring within the complete string S.
>     *   The input property is set to S.
>     *   The length property is set to n + 1.
>     *   The 0 property is set to the matched substring (i.e. the portion of S between offset i inclusive and offset e exclusive).
>     *   For each integer i such that I > 0 and I ≤ n, set the property named ToString(i) to the ith element of r's captures array.
