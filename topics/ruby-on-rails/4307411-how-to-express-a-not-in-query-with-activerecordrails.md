
# How to express a NOT IN query with ActiveRecord/Rails?

## Question
        
Just to update this since it seems a lot of people come to this, if you are using Rails 4 look at the answers by Trung Lê` and VinniVidiVicci.

    Topic.where.not(forum_id:@forums.map(&:id))
    
    Topic.where(published:true).where.not(forum_id:@forums.map(&:id))
    

I'm hoping there is a easy solution that doesn't involve `find_by_sql`, if not then I guess that will have to work.

I found [this article](http://trevorturk.com/2007/03/19/active-record-and-the-in-clause/) which references this:

    Topic.find(:all, :conditions => { :forum_id => @forums.map(&:id) })
    

which is the same as

    SELECT * FROM topics WHERE forum_id IN (<@forum ids>)
    

I am wondering if there is a way to do `NOT IN` with that, like:

    SELECT * FROM topics WHERE forum_id NOT IN (<@forum ids>)

## Answer
        
I'm using this:

    Topic.where('id NOT IN (?)', Array.wrap(actions))
    

Where `actions` is an array with: `[1,2,3,4,5]`

Edit:

For Rails 4 notation:

    Article.where.not(title: ['Rails 3', 'Rails 5'])
