
# How do I detect a click outside an element?

## Question
        
I have some HTML menus, which I show completely when a user clicks on the head of these menus. I would like to hide these elements when the user clicks outside the menus' area.

Is something like this possible with jQuery?

    $("#menuscontainer").clickOutsideThisElement(function() {
        // Hide the menus
    });

## Answer
        
> NOTE: Using `stopEventPropagation()` is something that should be avoided as it breaks normal event flow in the DOM. See [this article](https://css-tricks.com/dangers-stopping-event-propagation/) for more information. Consider using [this method](https://stackoverflow.com/a/3028037/561309) instead.

Attach a click event to the document body which closes the window. Attach a separate click event to the container which stops propagation to the document body.

    $(window).click(function() {
    //Hide the menus if visible
    });
    
    $('#menucontainer').click(function(event){
        event.stopPropagation();
    });
