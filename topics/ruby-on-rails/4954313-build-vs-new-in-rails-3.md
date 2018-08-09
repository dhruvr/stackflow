
# Build vs new in Rails 3

## Question
        
In the Rails 3 [docs](http://api.rubyonrails.org/classes/ActiveRecord/Associations/ClassMethods.html), the `build` method for associations is described as being the same as the `new` method, but with the automatic assignment of the foreign key. Straight from the docs:

    Firm#clients.build (similar to Client.new("firm_id" => id))
    

I've read similar elsewhere.

However, when I use `new` (e.g. `some_firm.clients.new` without any parameters), the new client's `firm_id` association **is** automatically created. I'm staring at the results right now in the console!

Am I missing something? Are the docs a bit out of date (unlikely)? What's the difference between `build` and `new`?

## Answer
        
You're misreading the docs slightly. `some_firm.client.new` is creating a new `Client` object from the clients collection, and so it can automatically set the `firm_id` to `some_firm.id`, whereas the docs are calling `Client.new` which has no knowledge of any Firm's id at all, so it needs the `firm_id` passed to it.

The only difference between `some_firm.clients.new` and `some_firm.clients.build` seems to be that `build` also adds the newly-created client to the `clients` collection:

    henrym:~/testapp$ rails c
    Loading development environment (Rails 3.0.4)
    r:001 > (some_firm = Firm.new).save # Create and save a new Firm
    #=> true 
    r:002 > some_firm.clients           # No clients yet
    #=> [] 
    r:003 > some_firm.clients.new       # Create a new client
    #=> #<Client id: nil, firm_id: 1, created_at: nil, updated_at: nil> 
    r:004 > some_firm.clients           # Still no clients
    #=> [] 
    r:005 > some_firm.clients.build     # Create a new client with build
    #=> #<Client id: nil, firm_id: 1, created_at: nil, updated_at: nil> 
    r:006 > some_firm.clients           # New client is added to clients 
    #=> [#<Client id: nil, firm_id: 1, created_at: nil, updated_at: nil>] 
    r:007 > some_firm.save
    #=> true 
    r:008 > some_firm.clients           # Saving firm also saves the attached client
    #=> [#<Client id: 1, firm_id: 1, created_at: "2011-02-11 00:18:47",
    updated_at: "2011-02-11 00:18:47">] 
    

If you're creating an object through an association, `build` should be preferred over `new` as build keeps your in-memory object, `some_firm` (in this case) in a consistent state even before any objects have been saved to the database.
