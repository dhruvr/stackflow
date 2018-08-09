
# Rails ActiveRecord date between

## Question
        
I need to query comments made in one day. The field is part of the standard timestamps, is created\_at. The selected date is coming from a date\_select. How can I use ActiveRecord to do that?

I need somthing like:

    "SELECT * FROM comments WHERE created_at BETWEEN '2010-02-03 00:00:00' AND '2010-02-03 23:59:59'"

## Answer
        
Just a note that the currently accepted answer is deprecated in Rails 3. You should do this instead:

    Comment.where(:created_at => @selected_date.beginning_of_day..@selected_date.end_of_day)
    

Or, if you want to or have to use [pure string conditions](http://guides.rubyonrails.org/active_record_querying.html#pure-string-conditions), you can do:

    Comment.where('created_at BETWEEN ? AND ?', @selected_date.beginning_of_day, @selected_date.end_of_day)
