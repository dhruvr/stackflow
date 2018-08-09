
# jQuery scroll to element

## Question
        
I have this `input` element:

    <input type="text" class="textfield" value="" id="subject" name="subject">
    

Then I have some other elements, like other text inputs, textareas, etc.

When the user clicks on that `input` with `#subject`, the page should scroll to the last element of the page with a nice animation. It should be a scroll to bottom and not to top.

The last item of the page is a `submit` button with `#submit`:

    <input type="submit" class="submit" id="submit" name="submit" value="Ok, Done.">
    

The animation should not be too fast and should be fluid.

I am running the latest jQuery version. I prefer to not install any plugin but to use the default jQuery features to achieve this.

## Answer
        
Assuming you have a button with the id `button`, try this example:

    $("#button").click(function() {
        $([document.documentElement, document.body]).animate({
            scrollTop: $("#elementtoScrollToID").offset().top
        }, 2000);
    });
    

I got the code from the article _[Smoothly scroll to an element without a jQuery plugin](http://www.abeautifulsite.net/smoothly-scroll-to-an-element-without-a-jquery-plugin-2/)_. And I have tested it on the example below.

    <html>
        <script src="https://ajax.googleapis.com/ajax/libs/jquery/1.5.1/jquery.min.js"></script>
        <script>
            $(document).ready(function (){
                $("#click").click(function (){
                    $('html, body').animate({
                        scrollTop: $("#div1").offset().top
                    }, 2000);
                });
            });
        </script>
        <div id="div1" style="height: 1000px; width 100px">
            Test
        </div>
        <br/>
        <div id="div2" style="height: 1000px; width 100px">
            Test 2
        </div>
        <button id="click">Click me</button>
    </html>
