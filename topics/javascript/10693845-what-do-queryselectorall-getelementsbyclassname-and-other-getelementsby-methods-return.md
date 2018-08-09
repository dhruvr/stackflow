
# What do querySelectorAll, getElementsByClassName and other getElementsBy* methods return?

## Question
        
Do `getElementsByClassName` (and similar functions like `getElementsByTagName` and `querySelectorAll`) work the same as `getElementById` or do they return an array of elements?

The reason I ask is because I am trying to change the style of all elements using `getElementsByClassName`. See below.

    //doesn't work
    document.getElementsByClassName('myElement').style.size = '100px';
    
    //works
    document.getElementById('myIdElement').style.size = '100px';

## Answer
        
Your `getElementById()` code works since IDs have to be unique and thus the function always returns exactly one element (or `null` if none was found).

However, [`getElementsByClassName()`](https://developer.mozilla.org/en/DOM/document.getElementsByClassName#Syntax), [`querySelectorAll()`](https://developer.mozilla.org/en-US/docs/Web/API/Element/querySelectorAll), and other `getElementsBy*` methods return an array-like collection of elements. Iterate over it like you would with a real array:

    var elems = document.getElementsByClassName('myElement');
    for(var i = 0; i < elems.length; i++) {
        elems[i].style.size = '100px';
    }
    

If you prefer something shorter, consider using [jQuery](http://jquery.com):

    $('.myElement').css('size', '100px');
