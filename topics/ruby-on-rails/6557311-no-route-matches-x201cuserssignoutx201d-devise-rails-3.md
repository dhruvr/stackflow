
# No route matches &#x201C;/users/sign_out&#x201D; devise rails 3

## Question
        
I've installed devise on my app and applied the following in my `application.html.erb` file:

    <div id="user_nav">
        <% if user_signed_in? %>
            Signed in as <%= current_user.email %>. This cannot be cheese?
            <%= link_to 'Sign out', destroy_user_session_path %>
        <% else %>
            <%= link_to 'Register', new_user_registration_path %> or <%= link_to 'Sign in', new_user_session_path %>
        <% end %>
    </div>
    

I ran `rake routes` and confirmed that all the routes are valid.

Also, in my `routes.rb` file I have `devise_for :users` and `root :to => "home#index"`.

I get the following routing error when clicking the "Sign out" link:

    No route matches "/users/sign_out"
    

Any ideas what's causing the error?

## Answer
        
I think the route for signing out is a `DELETE` method. This means that your sign out link needs to look like this:

    <%= link_to "Sign out", destroy_user_session_path, :method => :delete %>
    

Yours doesn't include the `:method => :delete` part. Also, please note that for this to work you must also include `<%= javascript_include_tag :defaults %>` in your layout file (`application.html.erb`).
