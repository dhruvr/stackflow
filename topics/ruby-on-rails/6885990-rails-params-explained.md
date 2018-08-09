
# Rails params explained?

## Question
        
Could anyone explain `params` in Rails controller: where they come from, and what they are referencing?

      def create
        @vote = Vote.new(params[:vote])
        item = params[:vote][:item_id]
        uid = params[:vote][:user_id]
        @extant = Vote.find(:last, :conditions => ["item_id = ? AND user_id = ?", item, uid])
        last_vote_time = @extant.created_at unless @extant.blank?
        curr_time = Time.now
      end
    

I would like to be able to read this code line-by-line and understand what's going on.

## Answer
        
The params come from the user's browser when they request the page. For an HTTP GET request, which is the most common, the params are encoded in the url. For example, if a user's browser requested

[http://www.example.com/?foo=1&boo=octopus](http://www.example.com/?foo=1&boo=octopus)

then `params[:foo]` would be "1" and `params[:boo]` would be "octopus".

In HTTP/HTML, the params are really just a series of key-value pairs where the key and the value are strings, but Ruby on Rails has a special syntax for making the params be a hash with hashes inside. For example, if the user's browser requested

[http://www.example.com/?vote\[item\_id\]=1&vote\[user\_id\]=2](http://www.example.com/?vote[item_id]=1&vote[user_id]=2)

then `params[:vote]` would be a hash, `params[:vote][:item_id]` would be "1" and `params[:vote][:user_id]` would be "2".

The Ruby on Rails params are the equivalent of the [$_REQUEST array in PHP](http://php.net/manual/en/reserved.variables.request.php).
