
# How can I avoid running ActiveRecord callbacks?

## Question
        
I have some models that have after_save callbacks. Usually that's fine, but in some situations, like when creating development data, I want to save the models without having the callbacks run. Is there a simple way to do that? Something akin to...

    Person#save( :run_callbacks => false )
    

or

    Person#save_without_callbacks
    

I looked in the Rails docs and didn't find anything. However in my experience the Rails docs don't always tell the whole story.

UPDATE

I found [a blog post](http://web.archive.org/web/20120701221754/http://blog.viarails.net/2009/1/29/disabling-callbacks-in-an-activerecord-data-migration) that explains how you can remove callbacks from a model like this:

    Foo.after_save.clear
    

I couldn't find where that method is documented but it seems to work.

## Answer
        
This solution is Rails 2 only.

I just investigated this and I think I have a solution. There are two ActiveRecord private methods that you can use:

    update_without_callbacks
    create_without_callbacks
    

You're going to have to use send to call these methods. examples:

    p = Person.new(:name => 'foo')
    p.send(:create_without_callbacks)
    
    p = Person.find(1)
    p.send(:update_without_callbacks)
    

This is definitely something that you'll only really want to use in the console or while doing some random tests. Hope this helps!
