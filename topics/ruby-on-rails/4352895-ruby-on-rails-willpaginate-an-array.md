
# Ruby on Rails will_paginate an array

## Question
        
I was wondering if someone could explain how to use [will_paginate](https://github.com/mislav/will_paginate) on an array of objects?

For example, on my site I have an opinion section where users can rate the opinions. Here's a method I wrote to gather the users who have rated the opinion:

    def agree_list
      list = OpinionRating.find_all_by_opinion_id(params[:id])
      @agree_list = []
      list.each do |r|
        user = Profile.find(r.profile_id)
        @agree_list << user
      end
    end
    

Thank you

## Answer
        
[will_paginate](https://github.com/mislav/will_paginate) 3.0 is designed to take advantage of the new `ActiveRecord::Relation` in Rails 3, so it defines `paginate` only on relations by default. It can still work with an array, but you have to tell rails to require that part.

In a file in your `config/initializers` (I used `will_paginate_array_fix.rb`), add this

    require 'will_paginate/array'
    

Then you can use on arrays

    my_array.paginate(:page => x, :per_page => y)
