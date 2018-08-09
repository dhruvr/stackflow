
# Single table inheritance and where to use it in Rails

## Question
        
I am stuck in a weird Design problem,

I am working on a two type of profiles Models,

*   User profile (belongs to User)
*   others that are maintain in-site as "bots" (doesn't belong to anybody)

The typical OO behaviour of these two types of Profiles is same but only the important attributes/properties are common ( the very important ones 5-6 in number), others properties like "interests etc"(almost 10-15 properties) are not there for bot profiles

The coder who worked on this earlier created separate models/Controllers for bot profiles / User profiles which creates a lot of redundancy everywhere and also as expected hard to maintain, write tests etc.I wanted to DRY this up, atleast to solve some/all of these redundancy problems.

Somebody suggested Single Table Inheritance as a solution

Somebody suggested Use Polymorphic Associations instead.

what is the better approach. When do we actually use STI?

My own thought was STI is used best when attributes are same for Models and they differ in behaviour.

Thoughts about what can I do?

## Answer
        
Characterising STI as mostly useful when attributes are the same but behaviour differs is "about right", but possibly a little limiting. I like to use STI when there is, as the name suggests, a clear OO-style inheritance relationship, rather than the database-style relationship between objects of different types.

If there is common code between bots and users, I'd say STI sounds like a winner. If there's just some common attributes, it's probably less applicable but still worth having a go at.

I'm a pretty experimental person, so my recommendation is to give it a go. Branch your code and refactor the models into an STI relationship. See if it really does dry things up, or just swaps one set of headaches for some other problem.

One thing I think you won't see much benefit from is drying up your controllers. In my experience, STI models don't often translate into similarly related controllers. But that would be something else to experiment with. Sometimes there's a win, sometimes there isn't.
