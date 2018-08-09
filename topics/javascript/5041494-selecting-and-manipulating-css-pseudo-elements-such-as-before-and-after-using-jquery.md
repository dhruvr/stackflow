
# Selecting and manipulating CSS pseudo-elements such as ::before and ::after using jQuery

## Question
        
Is there any way to select/manipulate CSS pseudo-elements such as `::before` and `::after` (and the old version with one semi-colon) using jQuery?

For example, my stylesheet has the following rule:

    .span::after{ content:'foo' }
    

How can I change 'foo' to 'bar' using jQuery?

## Answer
        
You could also pass the content to the pseudo element with a data attribute and then use jQuery to manipulate that:

In HTML:

    <span>foo</span>
    

In jQuery:

    $('span').hover(function(){
        $(this).attr('data-content','bar');
    });
    

In CSS:

    span:after {
        content: attr(data-content) ' any other text you may want';
    }
    

If you want to prevent the 'other text' from showing up, you could combine this with seucolega's solution like this:

In HTML:

    <span>foo</span>
    

In jQuery:

    $('span').hover(function(){
        $(this).addClass('change').attr('data-content','bar');
    });
    

In CSS:

    span.change:after {
        content: attr(data-content) ' any other text you may want';
    }
