
# ActiveRecord: size vs count

## Question
        
In Rails, you can find the number of records using both `Model.size` and `Model.count`. If you're dealing with more complex queries is there any advantage to using one method over the other? How are they different?

For instance, I have users with photos. If I want to show a table of users and how many photos they have, will running many instances of `user.photos.size` be faster or slower than `user.photos.count`?

Thanks!

## Answer
        
You should read [that](http://web.archive.org/web/20100210204319/http://blog.hasmanythrough.com/2008/2/27/count-length-size), it's still valid.

You'll adapt the function you use depending on your needs.

Basically:

*   if you already load all entries, say `User.all`, then you should use `length` to avoid another db query
    
*   if you haven't anything loaded, use `count` to make a count query on your db
    
*   if you don't want to bother with these considerations, use `size` which will adapt
