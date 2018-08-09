
# Open a URL in a new tab (and not a new window) using JavaScript

## Question
        
I'm trying to open a [URL](http://en.wikipedia.org/wiki/Uniform_Resource_Locator) in a new tab, as opposed to a popup window. I've seen related questions where the responses would look something like:

    window.open(url,'_blank');
    window.open(url);
    

But none of them worked for me, the browser still tried to open a popup window.

## Answer
        
Nothing an author can do can choose to open in a new tab instead of a new window. It is a _user preference_.

CSS3 proposed [target-new](http://www.w3.org/TR/2004/WD-css3-hyperlinks-20040224/#target-new), but [the specification was abandoned](http://www.w3.org/TR/2014/NOTE-css3-hyperlinks-20141014/).

The **reverse** is not true; by specifying dimensions for the window in the third argument of `window.open`, you can trigger a new window when the preference is for tabs.
