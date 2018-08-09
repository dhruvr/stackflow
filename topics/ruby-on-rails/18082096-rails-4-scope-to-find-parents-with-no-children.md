
# Rails 4 scope to find parents with no children

## Question
        
I found one [answer](https://stackoverflow.com/a/2900187/385532) that had some usable `having` examples for finding parents with `n` children, but the same is not usable for finding parents with no children (presumably since the join excludes them).

    scope :with_children, joins(:children).group("child_join_table.parent_id").having("count(child_join_table.parent_id) > 0")
    

Can anyone point me in the right direction?

## Answer
        
This should do the job you want:

Rails 3 & 4
===========

    scope :without_children, includes(:children).where(:children => { :id => nil })
    

The big difference here is the `joins` becoming a `includes`: an include loads all the relations, if they exists, the join will load only the associated objects and ignore the object without a relation.

In fact, `scope :with_children, joins(:children)` should be just enough to return the Parent with at least 1 child. Try it out!

Rails 5
=======

> See @Anson's answer below

* * *

As @MauroDias pointed out, **if it is a self-referential relationship** between your parent and children, this code above won't work.

With a little bit of research, I found out how to do it:

Consider this model:

    class Item < ActiveRecord::Base
      has_many :children, :class_name => 'Item', :foreign_key => 'parent_id'
    

How to return all items with no child(ren):

    Item.includes(:children).where(children_items: { id: nil })
    

How did I find that `children_items` table?

`Item.joins(:children)` generates the following SQL:

    SELECT "items".* 
    FROM "items" 
     INNER JOIN "items" "children_items" 
     ON "children_items"."parent_id" = "items"."id"
    

So I guessed that Rails uses a table when in need of a JOIN in a self-referential case.

* * *

**Similar questions:**

*   [How to query a model based on attribute of another model which belongs to the first model?](https://stackoverflow.com/questions/23633301/how-to-query-a-model-based-on-attribute-of-another-model-which-belongs-to-the-fi/23633352#23633352)
*   [Rails active record querying association with 'exists'](https://stackoverflow.com/questions/18234602/rails-active-record-querying-association-with-exists/18234998#18234998)
*   [Rails 3, has\_one / has\_many with lambda condition](https://stackoverflow.com/questions/13515225/rails-3-has-one-has-many-with-lambda-condition/13516285#13516285)
*   [Join multiple tables with active records](https://stackoverflow.com/questions/24266069/join-multiple-tables-with-active-records/24266583#24266583)
