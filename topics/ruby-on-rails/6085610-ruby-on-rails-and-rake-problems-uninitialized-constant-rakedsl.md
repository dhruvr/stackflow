
# Ruby on Rails and Rake problems: uninitialized constant Rake::DSL

## Question
        
I'm having a really frustrating issue: [Rake](http://en.wikipedia.org/wiki/Rake_%28software%29) is being dumb.

Here's how the problem comes about:

    $ rails new test_app
    $ rails generate scaffold new_scaffold field1:string field2:text
    

Both of those work just fine, but then when I do this,

    $ rake db:migrate
    

I get the following error.

    (in /home/mikhail/test_app)
    rake aborted!
    uninitialized constant Rake::DSL
    /usr/lib/ruby/1.9.1/rake.rb:2482:in `const_missing'
    /usr/lib/ruby/gems/1.9.1/gems/rake-0.9.0/lib/rake/tasklib.rb:8:in `<class:TaskLib>'
    /usr/lib/ruby/gems/1.9.1/gems/rake-0.9.0/lib/rake/tasklib.rb:6:in `<module:Rake>'
    /usr/lib/ruby/gems/1.9.1/gems/rake-0.9.0/lib/rake/tasklib.rb:3:in `<top (required)>'
    /usr/lib/ruby/gems/1.9.1/gems/rake-0.9.0/lib/rake/rdoctask.rb:20:in `require'
    /usr/lib/ruby/gems/1.9.1/gems/rake-0.9.0/lib/rake/rdoctask.rb:20:in `<top (required)>'
    /usr/lib/ruby/gems/1.9.1/gems/railties-3.0.7/lib/rails/tasks/documentation.rake:1:in `require'
    /usr/lib/ruby/gems/1.9.1/gems/railties-3.0.7/lib/rails/tasks/documentation.rake:1:in `<top (required)>'
    /usr/lib/ruby/gems/1.9.1/gems/railties-3.0.7/lib/rails/tasks.rb:15:in `load'
    /usr/lib/ruby/gems/1.9.1/gems/railties-3.0.7/lib/rails/tasks.rb:15:in `block in <top (required)>'
    /usr/lib/ruby/gems/1.9.1/gems/railties-3.0.7/lib/rails/tasks.rb:6:in `each'
    /usr/lib/ruby/gems/1.9.1/gems/railties-3.0.7/lib/rails/tasks.rb:6:in `<top (required)>'
    /usr/lib/ruby/gems/1.9.1/gems/railties-3.0.7/lib/rails/application.rb:214:in `require'
    /usr/lib/ruby/gems/1.9.1/gems/railties-3.0.7/lib/rails/application.rb:214:in `initialize_tasks'
    /usr/lib/ruby/gems/1.9.1/gems/railties-3.0.7/lib/rails/application.rb:139:in `load_tasks'
    /usr/lib/ruby/gems/1.9.1/gems/railties-3.0.7/lib/rails/application.rb:77:in `method_missing'
    /home/mikhail/test_app/Rakefile:7:in `<top (required)>'
    /usr/lib/ruby/1.9.1/rake.rb:2373:in `load'
    /usr/lib/ruby/1.9.1/rake.rb:2373:in `raw_load_rakefile'
    /usr/lib/ruby/1.9.1/rake.rb:2007:in `block in load_rakefile'
    /usr/lib/ruby/1.9.1/rake.rb:2058:in `standard_exception_handling'
    /usr/lib/ruby/1.9.1/rake.rb:2006:in `load_rakefile'
    /usr/lib/ruby/1.9.1/rake.rb:1991:in `run'
    /usr/bin/rake:31:in `<main>'
    

I've looked about the Internet for similar/same errors, and people have had them. Just no one ever seems to solve the problem!

How do I fix this problem?

## Answer
        
A [tweet from DHH](http://twitter.com/#!/dhh/status/71966528744071169) earlier. Rake .9.0 breaks Rails and several other things, you need to:

    gem "rake", "0.8.7"
    

in your Gemfile.
