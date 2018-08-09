
# jQuery AJAX cross domain

## Question
        
Here are two pages, test.php and testserver.php.

**test.php**

    <script src="scripts/jq.js" type="text/javascript"></script>
    <script>
        $(function() {
            $.ajax({url:"testserver.php",
                success:function() {
                    alert("Success");
                },
                error:function() {
                    alert("Error");
                },
                dataType:"json",
                type:"get"
            }
        )})
    </script>
    

**testserver.php**

    <?php
    $arr = array("element1",
                 "element2",
                 array("element31","element32"));
    $arr['name'] = "response";
    echo json_encode($arr);
    ?>
    

Now my problem: when both of these files are on the same server (either localhost or web server), it works and `alert("Success")` is called; If it is on different servers, meaning testserver.php on web server and test.php on localhost, its not working, and `alert("Error")` is executing. Even if the URL inside ajax is changed to [http://domain.com/path/to/file/testserver.php](http://domain.com/path/to/file/testserver.php)

## Answer
        
Use [JSONP](http://en.wikipedia.org/wiki/JSONP).

jQuery:

    $.ajax({
         url:"testserver.php",
         dataType: 'jsonp', // Notice! JSONP <-- P (lowercase)
         success:function(json){
             // do stuff with json (in this case an array)
             alert("Success");
         },
         error:function(){
             alert("Error");
         }      
    });
    

PHP:

    <?php
    $arr = array("element1","element2",array("element31","element32"));
    $arr['name'] = "response";
    echo $_GET['callback']."(".json_encode($arr).");";
    ?>
    

The echo might be wrong, it's been a while since I've used php. In any case you need to output `callbackName('jsonString')` notice the quotes. jQuery will pass it's own callback name, so you need to get that from the GET params.

And as Stefan Kendall posted, [$.getJSON()](http://api.jquery.com/jQuery.getJSON/) is a shorthand method, but then you need to append `'callback=?'` to the url as GET parameter (yes, value is ?, jQuery replaces this with its own generated callback method).
