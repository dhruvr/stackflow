
# What is the difference between client-side and server-side programming?

## Question
        
I have this code:

    <script type="text/javascript">
        var foo = 'bar';
        <?php
            file_put_contents('foo.txt', ' + foo + ');
        ?>
    
        var baz = <?php echo 42; ?>;
        alert(baz);
    </script>
    

Why does this not write "bar" into my text file, but alerts "42"?

* * *

NB: Earlier revisions of this question were explicitly about PHP on the server and JavaScript on the client. The essential nature of the problem and solutions is the same for _any_ pair of languages when one is running on the client and the other on the server. Please take this in to account when you see answers talking about specific languages.

## Answer
        
Your code is split into two entirely separate parts, the _server side_ and the _client side_.

                        |
                   ---------->
                  HTTP request
                        |
    +--------------+    |    +--------------+
    |              |    |    |              |
    |    browser   |    |    |  web  server |
    | (JavaScript) |    |    |  (PHP etc.)  |
    |              |    |    |              |
    +--------------+    |    +--------------+
                        |
      client side       |      server side
                        |
                   <----------
              HTML, CSS, JavaScript
                        |
    

The two sides communicate via HTTP requests and responses. PHP is executed on the server and outputs some HTML and maybe JavaScript code which is sent as response to the client where the HTML is interpreted and the JavaScript is executed. Once PHP has finished outputting the response, the script ends and nothing will happen on the server until a new HTTP request comes in.

The example code executes like this:

    <script type="text/javascript">
        var foo = 'bar';
        <?php
            file_put_contents('foo.txt', ' + foo + ');
        ?>
    
        var baz = <?php echo 42; ?>;
        alert(baz);
    </script>
    

Step 1, PHP executes all code between `<?php ?>` tags. The result is this:

    <script type="text/javascript">
        var foo = 'bar';
    
        var baz = 42;
        alert(baz);
    </script>
    

The `file_put_contents` call did not result in anything, it just wrote " + foo + " into a file. The `<?php echo 42; ?>` call resulted in the output "42", which is now in the spot where that code used to be.

This resulting HTML/JavaScript code is now sent to the client, where it gets evaluated. The `alert` call works, while the `foo` variable is not used anywhere.

**All PHP code is executed on the server before the client even starts executing any of the JavaScript. There's no PHP code left in the response that JavaScript could interact with.**

To call some PHP code, the client will have to send a new HTTP request to the server. This can happen using one of three possible methods:

1.  A link, which causes the browser to load a new page.
2.  A form submission, which submits data to the server and loads a new page.
3.  An [AJAX](http://en.wikipedia.org/wiki/Ajax_%28programming%29) request, which is a Javascript technique to make a regular HTTP request to the server (like 1. and 2. will), but without leaving the current page.

[Here's a question outlining these method in greater detail](https://stackoverflow.com/questions/23740548/how-to-pass-variables-and-data-from-php-to-javascript/23741119)

You can also use JavaScript to make the browser open a new page using `window.location` or submit a form, emulating possibilities 1. and 2.
