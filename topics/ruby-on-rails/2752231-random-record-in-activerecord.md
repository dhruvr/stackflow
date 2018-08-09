
# Random record in ActiveRecord

## Question
        
I'm in need of getting a random record from a table via ActiveRecord. I've followed the example from [Jamis Buck from 2006](http://weblog.jamisbuck.org/2006/10/7/helping-activerecord-finders-help-you).

However, I've also come across another way via a Google search (can't attribute with a link due to new user restrictions):

     rand_id = rand(Model.count)
     rand_record = Model.first(:conditions => ["id >= ?", rand_id])
    

I'm curious how others on here have done it or if anyone knows what way would be more efficient.

## Answer
        
I haven't found an ideal way to do this without at least two queries.

The following uses a randomly generated number (up to the current record count) as an _offset_.

    offset = rand(Model.count)
    
    # Rails 4
    rand_record = Model.offset(offset).first
    
    # Rails 3
    rand_record = Model.first(:offset => offset)
    

To be honest, I've just been using ORDER BY RAND() or RANDOM() (depending on the database). It's not a performance issue if you don't have a performance issue.
