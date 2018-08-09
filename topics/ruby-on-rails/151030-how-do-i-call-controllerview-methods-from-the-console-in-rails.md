
# How do I call controller/view methods from the console in Rails?

## Question
        
When I load `script/console`, some times I want play with the output of a controller or a view helper method.

Are there ways to:

*   simulate a request?
*   call methods from a controller instance on said request?
*   test helper methods, either via said controller instance or another way?

## Answer
        
To call helpers, use the `helper` object:

    $ ./script/console
    >> helper.number_to_currency('123.45')
    => "R$ 123,45"
    

If you want to use a helper that's not included by default (say, because you removed `helper :all` from `ApplicationController`), just include the helper.

    >> include BogusHelper
    >> helper.bogus
    => "bogus output"
    

As for dealing with **controllers**, I quote [Nick's](https://stackoverflow.com/questions/151030/how-do-i-call-controller-view-methods-from-the-console-in-rails/1436342#1436342) answer:

>     > app.get '/posts/1'
>     > response = app.response
>     # you now have a rails response object much like the integration tests
>     
>     > response.body            # get you the HTML
>     > response.cookies         # hash of the cookies
>     
>     # etc, etc
>
