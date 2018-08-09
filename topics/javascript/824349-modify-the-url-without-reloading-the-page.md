
# Modify the URL without reloading the page

## Question
        
Is there any way I can modify the URL of the current page without reloading the page?

I would like to access the portion **before** the # hash if possible.

I only need to change the portion **after** the domain, so its not like I'm violating cross-domain policies.

     window.location.href = "www.mysite.com/page2.php";  // sadly this reloads

## Answer
        
This can now be done in Chrome, Safari, FF4+, and IE10pp4+!

See this question's answer for more info: [Updating address bar with new URL without hash or reloading the page](https://stackoverflow.com/questions/3338642/updating-address-bar-with-new-url-without-hash-or-reloading-the-page)

Example:

     function processAjaxData(response, urlPath){
         document.getElementById("content").innerHTML = response.html;
         document.title = response.pageTitle;
         window.history.pushState({"html":response.html,"pageTitle":response.pageTitle},"", urlPath);
     }
    

You can then use `window.onpopstate` to detect the back/forward button navigation:

    window.onpopstate = function(e){
        if(e.state){
            document.getElementById("content").innerHTML = e.state.html;
            document.title = e.state.pageTitle;
        }
    };
    

* * *

For a more in-depth look at manipulating browser history see [this MDN article](https://developer.mozilla.org/en-US/docs/Web/Guide/API/DOM/Manipulating_the_browser_history).
