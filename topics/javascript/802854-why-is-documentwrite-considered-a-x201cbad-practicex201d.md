
# Why is document.write considered a &#x201C;bad practice&#x201D;?

## Question
        
I know [`document.write`](https://developer.mozilla.org/en-US/docs/DOM/document.write) is considered bad practice; and I'm hoping to compile a list of reasons to submit to a 3rd party vendor as to why they shouldn't use `document.write` in implementations of their analytics code.

Please include your reason for claiming `document.write` as a bad practice below.

## Answer
        
A few of the more serious problems:

*   document.write (henceforth DW) does not work in XHTML
    
*   DW does not directly modify the DOM, preventing further manipulation _(trying to find evidence of this, but it's at best situational)_
    
*   DW executed after the page has finished loading will overwrite the page, or write a new page, or not work
    
*   DW executes where encountered: it cannot inject at a given node point
    
*   DW is effectively writing serialised text which is not the way the DOM works conceptually, and is an easy way to create bugs (.innerHTML has the same problem)
    

Far better to use the safe and DOM friendly [DOM manipulation methods](https://developer.mozilla.org/en-US/docs/Web/API/Document)
