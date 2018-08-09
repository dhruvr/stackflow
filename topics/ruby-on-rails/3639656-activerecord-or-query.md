
# ActiveRecord OR query

## Question
        
How do you do an OR query in Rails 3 ActiveRecord. All the examples I find just have AND queries.

> Edit: **OR** method is available since Rails 5. See [ActiveRecord::QueryMethods](http://api.rubyonrails.org/classes/ActiveRecord/QueryMethods.html#method-i-or)

## Answer
        
Use [ARel](http://github.com/rails/arel)

    t = Post.arel_table
    
    results = Post.where(
      t[:author].eq("Someone").
      or(t[:title].matches("%something%"))
    )
    

The resulting SQL:

    ree-1.8.7-2010.02 > puts Post.where(t[:author].eq("Someone").or(t[:title].matches("%something%"))).to_sql
    SELECT     "posts".* FROM       "posts"  WHERE     (("posts"."author" = 'Someone' OR "posts"."title" LIKE '%something%'))
