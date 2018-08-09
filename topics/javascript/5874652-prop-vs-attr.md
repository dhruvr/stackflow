
# .prop() vs .attr()

## Question
        
So [jQuery 1.6](http://api.jquery.com/category/version/1.6/) has the new function [`prop()`](http://api.jquery.com/prop/).

    $(selector).click(function(){
        //instead of:
        this.getAttribute('style');
        //do i use:
        $(this).prop('style');
        //or:
        $(this).attr('style');
    })
    

or in this case do they do the same thing?

And if I _do_ have to switch to using `prop()`, all the old `attr()` calls will break if i switch to 1.6?

**UPDATE**

    selector = '#id'
    
    $(selector).click(function() {
        //instead of:
        var getAtt = this.getAttribute('style');
        //do i use:
        var thisProp = $(this).prop('style');
        //or:
        var thisAttr = $(this).attr('style');
    
        console.log(getAtt, thisProp, thisAttr);
    });

    <script src="https://ajax.googleapis.com/ajax/libs/jquery/1.6.0/jquery.min.js"></script>
    <div id='id' style="color: red;background: orange;">test</div>

(see also this fiddle: [http://jsfiddle.net/maniator/JpUF2/](http://jsfiddle.net/maniator/JpUF2/))

The console logs the `getAttribute` as a string, and the `attr` as a string, but the `prop` as a `CSSStyleDeclaration`, Why? And how does that affect my coding in the future?

## Answer
        
**Update 1 November 2012**

My original answer applies specifically to jQuery 1.6. My advice remains the same but jQuery 1.6.1 changed things slightly: in the face of the predicted pile of broken websites, the jQuery team [reverted `attr()` to something close to (but not exactly the same as) its old behaviour for Boolean attributes](http://blog.jquery.com/2011/05/12/jquery-1-6-1-released/). John Resig also [blogged about it](http://ejohn.org/blog/jquery-16-and-attr/). I can see the difficulty they were in but still disagree with his recommendation to prefer `attr()`.

**Original answer**

If you've only ever used jQuery and not the DOM directly, this could be a confusing change, although it is definitely an improvement conceptually. Not so good for the bazillions of sites using jQuery that will break as a result of this change though.

I'll summarize the main issues:

*   You usually want `prop()` rather than `attr()`.
*   In the majority of cases, `prop()` does what `attr()` used to do. Replacing calls to `attr()` with `prop()` in your code will generally work.
*   Properties are generally simpler to deal with than attributes. An attribute value may only be a string whereas a property can be of any type. For example, the `checked` property is a Boolean, the `style` property is an object with individual properties for each style, the `size` property is a number.
*   Where both a property and an attribute with the same name exists, usually updating one will update the other, but this is not the case for certain attributes of inputs, such as `value` and `checked`: for these attributes, the property always represents the current state while the attribute (except in old versions of IE) corresponds to the default value/checkedness of the input (reflected in the `defaultValue` / `defaultChecked` property).
*   This change removes some of the layer of magic jQuery stuck in front of attributes and properties, meaning jQuery developers will have to learn a bit about the difference between properties and attributes. This is a good thing.

If you're a jQuery developer and are confused by this whole business about properties and attributes, you need to take a step back and learn a little about it, since jQuery is no longer trying so hard to shield you from this stuff. For the authoritative but somewhat dry word on the subject, there's the specs: [DOM4](http://www.w3.org/TR/dom/), [HTML DOM](http://www.w3.org/TR/DOM-Level-2-HTML/), [DOM Level 2](http://www.w3.org/TR/DOM-Level-2-Core), [DOM Level 3](http://www.w3.org/TR/DOM-Level-3-Core/). Mozilla's DOM documentation is valid for most modern browsers and is easier to read than the specs, so you may find their [DOM reference](https://developer.mozilla.org/en/gecko_dom_reference) helpful. There's a [section on element properties](https://developer.mozilla.org/en/DOM/element#Properties).

As an example of how properties are simpler to deal with than attributes, consider a checkbox that is initially checked. Here are two possible pieces of valid HTML to do this:

    <input id="cb" type="checkbox" checked>
    <input id="cb" type="checkbox" checked="checked">
    

So, how do you find out if the checkbox is checked with jQuery? Look on Stack Overflow and you'll commonly find the following suggestions:

*   `if ( $("#cb").attr("checked") === true ) {...}`
*   `if ( $("#cb").attr("checked") == "checked" ) {...}`
*   `if ( $("#cb").is(":checked") ) {...}`

This is actually the simplest thing in the world to do with the `checked` Boolean property, which has existed and worked flawlessly in every major scriptable browser since 1995:

`if (document.getElementById("cb").checked) {...}`

The property also makes checking or unchecking the checkbox trivial:

`document.getElementById("cb").checked = false`

In jQuery 1.6, this unambiguously becomes

`$("#cb").prop("checked", false)`

The idea of using the `checked` attribute for scripting a checkbox is unhelpful and unnecessary. The property is what you need.

*   It's not obvious what the correct way to check or uncheck the checkbox is using the `checked` attribute
*   The attribute value reflects the default rather than the current visible state (except in some older versions of IE, thus making things still harder). The attribute tells you nothing about the whether the checkbox on the page is checked. See [http://jsfiddle.net/VktA6/49/](http://jsfiddle.net/VktA6/49/).
