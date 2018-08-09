
# Facebook how to check if user has liked page and show content?

## Question
        
I am trying to create a Facebook iFrame app. The app should first show an image and if the user likes the page, he will get access to some content.

I use RoR, therefore I can't use the Facebook PhP SDK.

Here is my iFrame HTML when the user has not liked the page:

    <!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"
    "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
    <html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en">
    <head>
    <link rel="stylesheet" type="text/css" href="style.css" />
    <style type="text/css">
    body {
    width:520px;
    margin:0; padding:0; border:0;
    font-family: verdana;
    background:url(repeat.png) repeat;
    margin-bottom:10px;
    }
    p, h1 {width:450px; margin-left:50px; color:#FFF;}
    p {font-size:11px;}
    </style>
    <meta http-equiv="Content-Type" content="text/html;
    charset=iso-8859-1" />
    </head>
    <body>
    <div id="container">
    <img src="welcome.png" alt="Frontimg">
    </div>
    

And, if the user has liked the page:

    <!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"
    "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
    <html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en">
    <head>
    <link rel="stylesheet" type="text/css" href="style.css" />
    <style type="text/css">
    body {
    width:520px;
    margin:0; padding:0; border:0;
    font-family: verdana;
    background:url(repeat.png) repeat;
    margin-bottom:10px;
    }
    p, h1 {width:450px; margin-left:50px; color:#FFF;}
    p {font-size:11px;}
    </style>
    <meta http-equiv="Content-Type" content="text/html;
    charset=iso-8859-1" />
    </head>
    <body>
    <div id="container">
    <img src="member.png" alt="Frontimg">
    <p>You liked this page</p>

## Answer
        
**UPDATE 21/11/2012** @ALL : I have updated the example so that it works better and takes into accounts remarks from Chris Jacob and FB Best practices, have a look of working example [here](http://jsfiddle.net/X4bn6/1042/)

* * *

Hi So as promised here is my answer using only javascript :

The content of the BODY of the page :

    <div id="fb-root"></div>
    <script src="http://connect.facebook.net/en_US/all.js"></script>
    <script>
      FB.init({
        appId  : 'YOUR APP ID',
        status : true, 
        cookie : true, 
        xfbml  : true  
      });
    </script>
    
    <div id="container_notlike">
    YOU DONT LIKE
    </div>
    
    <div id="container_like">
    YOU LIKE
    </div>
    

The CSS :

    body {
    width:520px;
    margin:0; padding:0; border:0;
    font-family: verdana;
    background:url(repeat.png) repeat;
    margin-bottom:10px;
    }
    p, h1 {width:450px; margin-left:50px; color:#FFF;}
    p {font-size:11px;}
    
    #container_notlike, #container_like {
        display:none
    }
    

And finally the javascript :

    $(document).ready(function(){
    
        FB.login(function(response) {
          if (response.session) {
    
              var user_id = response.session.uid;
              var page_id = "40796308305"; //coca cola
              var fql_query = "SELECT uid FROM page_fan WHERE page_id = "+page_id+"and uid="+user_id;
              var the_query = FB.Data.query(fql_query);
    
              the_query.wait(function(rows) {
    
                  if (rows.length == 1 && rows[0].uid == user_id) {
                      $("#container_like").show();
    
                      //here you could also do some ajax and get the content for a "liker" instead of simply showing a hidden div in the page.
    
                  } else {
                      $("#container_notlike").show();
                      //and here you could get the content for a non liker in ajax...
                  }
              });
    
    
          } else {
            // user is not logged in
          }
        });
    
    });
    

So what what does it do ?

First it logins to FB (if you already have the USER ID, and you are sure your user is already logged in facebook, you can bypass the login stuff and replace `response.session.uid` with YOUR\_USER\_ID (from your rails app for example)

After that it makes a FQL query on the `page_fan` table, and the meaning is that if the user is a fan of the page, it returns the user id and otherwise it returns an empty array, after that and depending on the results its show a div or the other.

Also there is a working demo here : [http://jsfiddle.net/dwarfy/X4bn6/](http://jsfiddle.net/dwarfy/X4bn6/)

It's using the coca-cola page as an example, try it go and like/unlike [the coca cola page](http://www.facebook.com/cocacola) and run it again ...

Finally some related docs :

[FQL page_fan table](http://developers.facebook.com/docs/reference/fql/page_fan/)

[FBJS FB.Data.query](http://developers.facebook.com/docs/reference/javascript/FB.Data.query/)

Don't hesitate if you have any question ..

Cheers

**UPDATE 2**

As stated by somebody, jQuery is required for the javascript version to work BUT you could easily remove it (it's only used for the document.ready and show/hide).

For the document.ready, you could wrap your code in a function and use `body onload="your_function"` or something more complicated like here : [Javascript - How to detect if document has loaded (IE 7/Firefox 3)](https://stackoverflow.com/questions/978740/javascript-how-to-detect-if-document-has-loaded-ie-7-firefox-3) so that we replace document ready.

And for the show and hide stuff you could use something like : `document.getElementById("container_like").style.display = "none" or "block"` and for more reliable cross browser techniques see here : [http://www.webmasterworld.com/forum91/441.htm](http://www.webmasterworld.com/forum91/441.htm)

But jQuery is so easy :)

**UPDATE**

Relatively to the comment I posted here below here is some ruby code to decode the "signed_request" that facebook POST to your CANVAS URL when it fetches it for display inside facebook.

In your action controller :

    decoded_request = Canvas.parse_signed_request(params[:signed_request])
    

And then its a matter of checking the decoded request and display one page or another .. (Not sure about this one, I'm not comfortable with ruby)

    decoded_request['page']['liked']
    

And here is the related Canvas Class (from [fbgraph ruby library](https://github.com/nsanta/fbgraph/)) :

     class Canvas
    
        class << self
          def parse_signed_request(secret_id,request)
            encoded_sig, payload = request.split('.', 2)
            sig = ""
            urldecode64(encoded_sig).each_byte { |b|
              sig << "%02x" % b
            }
            data = JSON.parse(urldecode64(payload))
              if data['algorithm'].to_s.upcase != 'HMAC-SHA256'
              raise "Bad signature algorithm: %s" % data['algorithm']
            end
            expected_sig = OpenSSL::HMAC.hexdigest('sha256', secret_id, payload)
            if expected_sig != sig
              raise "Bad signature"
            end
            data
          end
    
          private
    
          def urldecode64(str)
            encoded_str = str.gsub('-','+').gsub('_','/')
            encoded_str += '=' while !(encoded_str.size % 4).zero?
            Base64.decode64(encoded_str)
          end
        end  
    
     end
