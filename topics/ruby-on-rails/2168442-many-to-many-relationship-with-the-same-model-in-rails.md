
# Many-to-many relationship with the same model in rails?

## Question
        
How can I make a many-to-many relationship with the same model in rails?

For example, each post is connected to many posts.

## Answer
        
There are several kinds of many-to-many relationships; you have to ask yourself the following questions:

*   Do I want to store additional information with the association? (Additional fields in the join table.)
*   Do the associations need to be implicitly bi-directional? (If post A is connected to post B, then post B is also connected to post A.)

That leaves four different possibilities. I'll walk over these below.

For reference: [the Rails documentation on the subject](http://api.rubyonrails.org/classes/ActiveRecord/Associations/ClassMethods.html). There's a section called “Many-to-many”, and of course the documentation on the class methods themselves.

Simplest scenario, uni-directional, no additional fields
--------------------------------------------------------

This is the most compact in code.

I'll start out with this basic schema for your posts:

    create_table "posts", :force => true do |t|
      t.string  "name", :null => false
    end
    

For any many-to-many relationship, you need a join table. Here's the schema for that:

    create_table "post_connections", :force => true, :id => false do |t|
      t.integer "post_a_id", :null => false
      t.integer "post_b_id", :null => false
    end
    

By default, Rails will call this table a combination of the names of the two tables we're joining. But that would turn out as `posts_posts` in this situation, so I decided to take `post_connections` instead.

Very important here is `:id => false`, to omit the default `id` column. Rails wants that column everywhere **except** on join tables for `has_and_belongs_to_many`. It will complain loudly.

Finally, notice that the column names are non-standard as well (not `post_id`), to prevent conflict.

Now in your model, you simply need to tell Rails about these couple of non-standard things. It will look as follows:

    class Post < ActiveRecord::Base
      has_and_belongs_to_many(:posts,
        :join_table => "post_connections",
        :foreign_key => "post_a_id",
        :association_foreign_key => "post_b_id")
    end
    

And that should simply work! Here's an example irb session run through `script/console`:

    >> a = Post.create :name => 'First post!'
    => #<Post id: 1, name: "First post!">
    >> b = Post.create :name => 'Second post?'
    => #<Post id: 2, name: "Second post?">
    >> c = Post.create :name => 'Definitely the third post.'
    => #<Post id: 3, name: "Definitely the third post.">
    >> a.posts = [b, c]
    => [#<Post id: 2, name: "Second post?">, #<Post id: 3, name: "Definitely the third post.">]
    >> b.posts
    => []
    >> b.posts = [a]
    => [#<Post id: 1, name: "First post!">]
    

You'll find that assigning to the `posts` association will create records in the `post_connections` table as appropriate.

Some things to note:

*   You can see in the above irb session that the association is uni-directional, because after `a.posts = [b, c]`, the output of `b.posts` does not include the first post.
*   Another thing you may have noticed is that there is no model `PostConnection`. You normally don't use models for a `has_and_belongs_to_many` association. For this reason, you won't be able to access any additional fields.

Uni-directional, with additional fields
---------------------------------------

Right, now... You've got a regular user who has today made a post on your site about how eels are delicious. This total stranger comes around to your site, signs up, and writes a scolding post on regular user's ineptitude. After all, eels are an endangered species!

So you'd like to make clear in your database that post B is a scolding rant on post A. To do that, you want to add a `category` field to the association.

What we need is no longer a `has_and_belongs_to_many`, but a combination of `has_many`, `belongs_to`, `has_many ..., :through => ...` and an extra model for the join table. This extra model is what gives us the power to add additional information to the association itself.

Here's another schema, very similar to the above:

    create_table "posts", :force => true do |t|
      t.string  "name", :null => false
    end
    
    create_table "post_connections", :force => true do |t|
      t.integer "post_a_id", :null => false
      t.integer "post_b_id", :null => false
      t.string  "category"
    end
    

Notice how, in this situation, `post_connections` **does** have an `id` column. (There's **no** `:id => false` parameter.) This is required, because there'll be a regular ActiveRecord model for accessing the table.

I'll start with the `PostConnection` model, because it's dead simple:

    class PostConnection < ActiveRecord::Base
      belongs_to :post_a, :class_name => :Post
      belongs_to :post_b, :class_name => :Post
    end
    

The only thing going on here is `:class_name`, which is necessary, because Rails cannot infer from `post_a` or `post_b` that we're dealing with a Post here. We have to tell it explicitly.

Now the `Post` model:

    class Post < ActiveRecord::Base
      has_many :post_connections, :foreign_key => :post_a_id
      has_many :posts, :through => :post_connections, :source => :post_b
    end
    

With the first `has_many` association, we tell the model to join `post_connections` on `posts.id = post_connections.post_a_id`.

With the second association, we are telling Rails that we can reach the other posts, the ones connected to this one, through our first association `post_connections`, followed by the `post_b` association of `PostConnection`.

There's just _one more thing_ missing, and that is that we need to tell Rails that a `PostConnection` is dependent on the posts it belongs to. If one or both of `post_a_id` and `post_b_id` were `NULL`, then that connection wouldn't tell us much, would it? Here's how we do that in our `Post` model:

    class Post < ActiveRecord::Base
      has_many(:post_connections, :foreign_key => :post_a_id, :dependent => :destroy)
      has_many(:reverse_post_connections, :class_name => :PostConnection,
          :foreign_key => :post_b_id, :dependent => :destroy)
    
      has_many :posts, :through => :post_connections, :source => :post_b
    end
    

Besides the slight change in syntax, two real things are different here:

*   The `has_many :post_connections` has an extra `:dependent` parameter. With the value `:destroy`, we tell Rails that, once this post disappears, it can go ahead and destroy these objects. An alternative value you can use here is `:delete_all`, which is faster, but will not call any destroy hooks if you are using those.
*   We've added a `has_many` association for the **reverse** connections as well, the ones that have linked us through `post_b_id`. This way, Rails can neatly destroy those as well. Note that we have to specify `:class_name` here, because the model's class name can no longer be inferred from `:reverse_post_connections`.

With this in place, I bring you another irb session through `script/console`:

    >> a = Post.create :name => 'Eels are delicious!'
    => #<Post id: 16, name: "Eels are delicious!">
    >> b = Post.create :name => 'You insensitive cloth!'
    => #<Post id: 17, name: "You insensitive cloth!">
    >> b.posts = [a]
    => [#<Post id: 16, name: "Eels are delicious!">]
    >> b.post_connections
    => [#<PostConnection id: 3, post_a_id: 17, post_b_id: 16, category: nil>]
    >> connection = b.post_connections[0]
    => #<PostConnection id: 3, post_a_id: 17, post_b_id: 16, category: nil>
    >> connection.category = "scolding"
    => "scolding"
    >> connection.save!
    => true
    

Instead of creating the association and then setting the category separately, you can also just create a PostConnection and be done with it:

    >> b.posts = []
    => []
    >> PostConnection.create(
    ?>   :post_a => b, :post_b => a,
    ?>   :category => "scolding"
    >> )
    => #<PostConnection id: 5, post_a_id: 17, post_b_id: 16, category: "scolding">
    >> b.posts(true)  # 'true' means force a reload
    => [#<Post id: 16, name: "Eels are delicious!">]
    

And we can also manipulate the `post_connections` and `reverse_post_connections` associations; it will neatly reflect in the `posts` association:

    >> a.reverse_post_connections
    => #<PostConnection id: 5, post_a_id: 17, post_b_id: 16, category: "scolding">
    >> a.reverse_post_connections = []
    => []
    >> b.posts(true)  # 'true' means force a reload
    => []
    

Bi-directional looped associations
----------------------------------

In normal `has_and_belongs_to_many` associations, the association is defined in **both** models involved. And the association is bi-directional.

But there is just one Post model in this case. And the association is only specified once. That's exactly why in this specific case, associations are uni-directional.

The same is true for the alternative method with `has_many` and a model for the join table.

This is best seen when simply accessing the associations from irb, and looking at the SQL that Rails generates in the log file. You'll find something like the following:

    SELECT * FROM "posts"
    INNER JOIN "post_connections" ON "posts".id = "post_connections".post_b_id
    WHERE ("post_connections".post_a_id = 1 )
    

To make the association bi-directional, we'd have to find a way to make Rails `OR` the above conditions with `post_a_id` and `post_b_id` reversed, so it will look in both directions.

Unfortunately, the only way to do this that I know of is rather hacky. You'll have to manually specify your SQL using options to `has_and_belongs_to_many` such as `:finder_sql`, `:delete_sql`, etc. It's not pretty. (I'm open to suggestions here too. Anyone?)
