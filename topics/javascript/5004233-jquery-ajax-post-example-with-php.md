
# jQuery Ajax POST example with PHP

## Question
        
I am trying to send data from a form to a database. Here is the form I am using:

    <form name="foo" action="form.php" method="POST" id="foo">
        <label for="bar">A bar</label>
        <input id="bar" name="bar" type="text" value="" />
        <input type="submit" value="Send" />
    </form>
    

The typical approach would be to submit the form, but this causes the browser to redirect. Using jQuery and [Ajax](http://en.wikipedia.org/wiki/Ajax_%28programming%29), is it possible to capture all of the form's data and submit it to a PHP script (in example, _form.php_)?

## Answer
        
Basic usage of [`.ajax`](https://api.jquery.com/jQuery.ajax/) would look something like this:

**HTML:**

    <form id="foo">
        <label for="bar">A bar</label>
        <input id="bar" name="bar" type="text" value="" />
    
        <input type="submit" value="Send" />
    </form>
    

**JQuery:**

    // Variable to hold request
    var request;
    
    // Bind to the submit event of our form
    $("#foo").submit(function(event){
    
        // Prevent default posting of form - put here to work in case of errors
        event.preventDefault();
    
        // Abort any pending request
        if (request) {
            request.abort();
        }
        // setup some local variables
        var $form = $(this);
    
        // Let's select and cache all the fields
        var $inputs = $form.find("input, select, button, textarea");
    
        // Serialize the data in the form
        var serializedData = $form.serialize();
    
        // Let's disable the inputs for the duration of the Ajax request.
        // Note: we disable elements AFTER the form data has been serialized.
        // Disabled form elements will not be serialized.
        $inputs.prop("disabled", true);
    
        // Fire off the request to /form.php
        request = $.ajax({
            url: "/form.php",
            type: "post",
            data: serializedData
        });
    
        // Callback handler that will be called on success
        request.done(function (response, textStatus, jqXHR){
            // Log a message to the console
            console.log("Hooray, it worked!");
        });
    
        // Callback handler that will be called on failure
        request.fail(function (jqXHR, textStatus, errorThrown){
            // Log the error to the console
            console.error(
                "The following error occurred: "+
                textStatus, errorThrown
            );
        });
    
        // Callback handler that will be called regardless
        // if the request failed or succeeded
        request.always(function () {
            // Reenable the inputs
            $inputs.prop("disabled", false);
        });
    
    });
    

_Note: Since jQuery 1.8, `.success()`, `.error()` and `.complete()` are deprecated in favor of `.done()`, `.fail()` and `.always()`._

_Note: Remember that the above snippet has to be done after DOM ready, so you should put it inside a [`$(document).ready()`](https://api.jquery.com/ready/) handler (or use the `$()` shorthand)._

_Tip: You can [chain](http://www.jquerybyexample.net/2012/06/what-is-chaining-in-jquery.html) the callback handlers like this: `$.ajax().done().fail().always();`_

**PHP (that is, form.php):**

    // You can access the values posted by jQuery.ajax
    // through the global variable $_POST, like this:
    $bar = isset($_POST['bar']) ? $_POST['bar'] : null;
    

_Note: Always [sanitize posted data](https://stackoverflow.com/questions/1314518/sanitizing-users-data-in-get-by-php), to prevent injections and other malicious code._

You could also use the shorthand [`.post`](https://api.jquery.com/jQuery.post/) in place of `.ajax` in the above JavaScript code:

    $.post('/form.php', serializedData, function(response) {
        // Log the response to the console
        console.log("Response: "+response);
    });
    

_Note: The above JavaScript code is made to work with jQuery 1.8 and later, but it should work with previous versions down to jQuery 1.5._
