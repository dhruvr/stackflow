
# How to override to_json in Rails?

## Question
        
* * *

Update:
=======

This issue was not properly explored. The real issue lies within `render :json`.

The first code paste in the original question will yield the expected result. However, there is still a caveat. See this example:

`render :json => current_user`

is **NOT** the same as

`render :json => current_user.to_json`

That is, `render :json` will not automatically call the `to_json` method associated with the User object. _In fact_, if `to_json` is being overridden on the `User` model, `render :json => @user` will generate the `ArgumentError` described below.

summary
-------

    # works if User#to_json is not overridden
    render :json => current_user
    
    # If User#to_json is overridden, User requires explicit call
    render :json => current_user.to_json
    

This all seems silly to me. This seems to be telling me that `render` is not actually calling `Model#to_json` when type `:json` is specified. Can someone explain what's really going on here?

Any genii that can help me with this can likely answer my other question: [How to build a JSON response by combining @foo.to\_json(options) and @bars.to\_json(options) in Rails](https://stackoverflow.com/questions/2572340/how-to-build-a-json-response-by-combining-foo-to-jsonoptions-and-bars-to-json)

* * *

Original Question:
==================

I've seen some other examples on SO, but I none do what I'm looking for.

I'm trying:

    class User < ActiveRecord::Base
    
      # this actually works! (see update summary above)
      def to_json
        super(:only => :username, :methods => [:foo, :bar])
      end
    
    end
    

I'm getting `ArgumentError: wrong number of arguments (1 for 0)` in

    /usr/lib/ruby/gems/1.9.1/gems/activesupport-2.3.5/lib/active_support/json/encoders/object.rb:4:in `to_json
    

Any ideas?

## Answer
        
You are getting `ArgumentError: wrong number of arguments (1 for 0)` because `to_json` needs to be overridden with one parameter, the `options` hash.

    def to_json(options)
      ...
    end
    

* * *

**Longer explanation of `to_json`, `as_json`, and rendering:**

In ActiveSupport 2.3.3, `as_json` was added to address issues like the one you have encountered. The _creation_ of the json should be separate from the _rendering_ of the json.

Now, anytime `to_json` is called on an object, `as_json` is invoked to create the data structure, and then that hash is encoded as a JSON string using `ActiveSupport::json.encode`. This happens for all types: object, numeric, date, string, etc (see the ActiveSupport code).

ActiveRecord objects behave the same way. There is a default `as_json` implementation that creates a hash that includes all the model's attributes. **You should override `as_json` in your Model to create the JSON structure you want**. `as_json`, just like the old `to_json`, takes an option hash where you can specify attributes and methods to include declaratively.

    def as_json(options)
      # this example ignores the user's options
      super(:only => [:email, :handle])
    end
    

In your controller, `render :json => o` can accept a string or an object. **If it's a string, it's passed through as the response body, if it's an object, `to_json` is called, which triggers `as_json` as explained above.**

So, as long as your models are properly represented with `as_json` overrides (or not), your controller code to display one model should look like this:

    format.json { render :json => @user }
    

The moral of the story is: **Avoid calling `to_json` directly, allow `render` to do that for you. If you need to tweak the JSON output, call `as_json`.**

    format.json { render :json => 
        @user.as_json(:only => [:username], :methods => [:avatar]) }
