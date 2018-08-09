
# How to validate an email address in JavaScript?

## Question
        
How can an email address be validated in JavaScript?

## Answer
        
Using [regular expressions](http://en.wikipedia.org/wiki/Regular_expression) is probably the best way. You can see a bunch of tests [here](http://jsfiddle.net/ghvj4gy9/embedded/result,js/) (taken from [chromium](https://code.google.com/p/chromium/codesearch#chromium/src/third_party/WebKit/LayoutTests/fast/forms/resources/ValidityState-typeMismatch-email.js&sq=package:chromium&type=cs))

    function validateEmail(email) {
        var re = /^(([^<>()\[\]\\.,;:\s@"]+(\.[^<>()\[\]\\.,;:\s@"]+)*)|(".+"))@((\[[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\])|(([a-zA-Z\-0-9]+\.)+[a-zA-Z]{2,}))$/;
        return re.test(String(email).toLowerCase());
    }
    

Here's the example of regular expresion that accepts unicode:

    var re = /^(([^<>()\[\]\.,;:\s@\"]+(\.[^<>()\[\]\.,;:\s@\"]+)*)|(\".+\"))@(([^<>()[\]\.,;:\s@\"]+\.)+[^<>()[\]\.,;:\s@\"]{2,})$/i;
    

But keep in mind that one should not rely only upon JavaScript validation. JavaScript can easily be disabled. This should be validated on the server side as well.

Here's an example of the above in action:

    function validateEmail(email) {
      var re = /^(([^<>()[\]\\.,;:\s@\"]+(\.[^<>()[\]\\.,;:\s@\"]+)*)|(\".+\"))@((\[[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\])|(([a-zA-Z\-0-9]+\.)+[a-zA-Z]{2,}))$/;
      return re.test(email);
    }
    
    function validate() {
      var $result = $("#result");
      var email = $("#email").val();
      $result.text("");
    
      if (validateEmail(email)) {
        $result.text(email + " is valid :)");
        $result.css("color", "green");
      } else {
        $result.text(email + " is not valid :(");
        $result.css("color", "red");
      }
      return false;
    }
    
    $("#validate").bind("click", validate);

    <script src="https://ajax.googleapis.com/ajax/libs/jquery/2.1.1/jquery.min.js"></script>
    
    <form>
      <p>Enter an email address:</p>
      <input id='email'>
      <button type='submit' id='validate'>Validate!</button>
    </form>
    
    <h2 id='result'></h2>
