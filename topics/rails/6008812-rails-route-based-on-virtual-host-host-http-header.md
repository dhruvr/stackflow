
# Rails route based on virtual host (Host HTTP header)

## Question
      
Is it possible to specify a Ruby on Rails route based on the host part of the request URL?

In config/routes.rb, I have

    root :to => 'entities#index'
    

but I would like to use the same code base to serve several sites, each with their own default controller.
## Answer
      
You can use [Request Base Constraints](http://guides.rubyonrails.org/routing.html#request-based-constraints)

    root :to => "siteone#index", :constraints => {:host => "siteone"}
    root :to => "sitetwo#index", :constraints => {:host => "sitetwo"}
    

Hope this helps.
    