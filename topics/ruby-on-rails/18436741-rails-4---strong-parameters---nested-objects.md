
# Rails 4 - Strong Parameters - Nested Objects

## Question
        
I've got a pretty simple question. But haven't found a solution so far.

So here's the JSON string I send to the server:

    {
      "name" : "abc",
      "groundtruth" : {
        "type" : "Point",
        "coordinates" : [ 2.4, 6 ]
      }
    }
    

Using the new permit method, I've got:

    params.require(:measurement).permit(:name, :groundtruth)
    

This throws no errors, but the created database entry contains `null` instead of the groundtruth value.

If I just set:

    params.require(:measurement).permit!
    

Everything get's saved as expected, but of course, this kills the security provided by strong parameters.

I've found solutions, how to permit arrays, but not a single example using nested objects. This must be possible somehow, since it should be a pretty common use case. So, how does it work?

## Answer
        
As odd as it sound when you want to permit nested attributes you do specify the attributes of nested object within an array. In your case it would be

**Update** as suggested by @RafaelOliveira

    params.require(:measurement)
          .permit(:name, :groundtruth => [:type, :coordinates => []])
    

On the other hand if you want nested of multiple objects then you wrap it inside a hash… like this

    params.require(:foo).permit(:bar, {:baz => [:x, :y]})
    

  

Rails actually have pretty good documentation on this: [http://api.rubyonrails.org/classes/ActionController/Parameters.html#method-i-permit](http://api.rubyonrails.org/classes/ActionController/Parameters.html#method-i-permit)

For further clarification, you could look at the implementation of `permit` and `strong_parameters` itself: [https://github.com/rails/rails/blob/master/actionpack/lib/action\_controller/metal/strong\_parameters.rb#L246-L247](https://github.com/rails/rails/blob/master/actionpack/lib/action_controller/metal/strong_parameters.rb#L246-L247)
