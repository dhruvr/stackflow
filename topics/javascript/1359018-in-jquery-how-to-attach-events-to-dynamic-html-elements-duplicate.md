
# In jQuery, how to attach events to dynamic html elements? [duplicate]

## Question
        
This question already has an answer here:

*   [Event binding on dynamically created elements?](/questions/203198/event-binding-on-dynamically-created-elements) 23 answers

Suppose I have some jQuery code that attaches an event handler to all elements with class "myclass". For example:

    $(function(){
        $(".myclass").click( function() {
            // do something
        });
    });
    

And my html might be as follows:

    <a class="myclass" href="#">test1</a>
    <a class="myclass" href="#">test2</a>
    <a class="myclass" href="#">test3</a>
    

That works with no problem. However, consider if the "myclass" elements were written to the page at some future time.

For example:

    <a id="anchor1" href="#">create link dynamically</a>
    <script type="text/javascript">
    $(function(){
        $("#anchor1").click( function() {
            $("#anchor1").append('<a class="myclass" href="#">test4</a>');
        });
    });
    </script>
    

In this case, the "test4" link is created when a user clicks on a#anchor1.

The "test4" link does not have the click() handler associated with it, even though it has class="myclass".

Any idea how I can fix this?

Basically, I would like to write the click() handler once and have it apply to both content present at page load, and content brought in later via Ajax/DHTML.

## Answer
        
I am adding a new answer to reflect changes in later jQuery releases. The .live() method is deprecated as of jQuery 1.7.

From [http://api.jquery.com/live/](http://api.jquery.com/live/)

> As of jQuery 1.7, the .live() method is deprecated. Use .on() to attach event handlers. Users of older versions of jQuery should use .delegate() in preference to .live().

For jQuery 1.7+ you can attach an event handler to a parent element using .on(), and pass the a selector combined with 'myclass' as an argument.

See [http://api.jquery.com/on/](http://api.jquery.com/on/)

So instead of...

    $(".myclass").click( function() {
        // do something
    });
    

You can write...

    $('body').on('click', 'a.myclass', function() {
        // do something
    });
    

This will work for all a tags with 'myclass' in the body, whether already present or dynamically added later.

The body tag is used here as the example had no closer static surrounding tag, but any parent tag that exists when the .on method call occurs will work. For instance a ul tag for a list which will have dynamic elements added would look like this:

    $('ul').on('click', 'li', function() {
        alert( $(this).text() );
    });
    

As long as the ul tag exists this will work (no li elements need exist yet).
