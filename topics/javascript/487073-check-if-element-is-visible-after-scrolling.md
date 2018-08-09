
# Check if element is visible after scrolling

## Question
        
I'm loading elements via AJAX. Some of them are only visible if you scroll down the page.  
Is there any way I can know if an element is now in the visible part of the page?

## Answer
        
This should do the trick:

    function isScrolledIntoView(elem)
    {
        var docViewTop = $(window).scrollTop();
        var docViewBottom = docViewTop + $(window).height();
    
        var elemTop = $(elem).offset().top;
        var elemBottom = elemTop + $(elem).height();
    
        return ((elemBottom <= docViewBottom) && (elemTop >= docViewTop));
    }
    

**Simple Utility Function** This will allow you to call a utility function that accepts the element you're looking for and if you want the element to be fully in view or partially.

    function Utils() {
    
    }
    
    Utils.prototype = {
        constructor: Utils,
        isElementInView: function (element, fullyInView) {
            var pageTop = $(window).scrollTop();
            var pageBottom = pageTop + $(window).height();
            var elementTop = $(element).offset().top;
            var elementBottom = elementTop + $(element).height();
    
            if (fullyInView === true) {
                return ((pageTop < elementTop) && (pageBottom > elementBottom));
            } else {
                return ((elementTop <= pageBottom) && (elementBottom >= pageTop));
            }
        }
    };
    
    var Utils = new Utils();
    

**Usage**

    var isElementInView = Utils.isElementInView($('#flyout-left-container'), false);
    
    if (isElementInView) {
        console.log('in view');
    } else {
        console.log('out of view');
    }
