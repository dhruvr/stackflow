
# Is there a JavaScript/jQuery DOM change listener?

## Question
        
Essentially I want to have a script execute when the contents of a DIV change. Since the scripts are separate (content script in chrome extension & webpage script), I need a way simply observe changes in DOM state. I could set up polling but that seems sloppy.

## Answer
        
Several years later, there is now officially a better solution. [DOM4 Mutation Observers](https://dom.spec.whatwg.org/#interface-mutationobserver) are the replacement for deprecated DOM3 mutation events. They are [currently implemented in modern browsers](http://caniuse.com/#feat=mutationobserver) as `MutationObserver` (or as the vendor-prefixed `WebKitMutationObserver` in old versions of Chrome):

    MutationObserver = window.MutationObserver || window.WebKitMutationObserver;
    
    var observer = new MutationObserver(function(mutations, observer) {
        // fired when a mutation occurs
        console.log(mutations, observer);
        // ...
    });
    
    // define what element should be observed by the observer
    // and what types of mutations trigger the callback
    observer.observe(document, {
      subtree: true,
      attributes: true
      //...
    });
    

This example listens for DOM changes on `document` and its entire subtree, and it will fire on changes to element attributes as well as structural changes. The draft spec has a full list of valid [mutation listener properties](https://dom.spec.whatwg.org/#interface-mutationobserver):

> **childList**
> 
> *   Set to `true` if mutations to target's children are to be observed.
> 
> **attributes**
> 
> *   Set to `true` if mutations to target's attributes are to be observed.
> 
> **characterData**
> 
> *   Set to `true` if mutations to target's data are to be observed.
> 
> **subtree**
> 
> *   Set to `true` if mutations to not just target, but also target's descendants are to be observed.
> 
> **attributeOldValue**
> 
> *   Set to `true` if `attributes` is set to true and target's attribute value before the mutation needs to be recorded.
> 
> **characterDataOldValue**
> 
> *   Set to `true` if `characterData` is set to true and target's data before the mutation needs to be recorded.
> 
> **attributeFilter**
> 
> *   Set to a list of attribute local names (without namespace) if not all attribute mutations need to be observed.

(This list is current as of April 2014; you may check the specification for any changes.)
