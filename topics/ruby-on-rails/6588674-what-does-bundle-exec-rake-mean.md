
# What does bundle exec rake mean?

## Question
        
What does `bundle exec rake db:migrate` mean? Or just `bundle exec rake <command>` in general?

I understand that `bundle` takes care of maintaining things in the Gemfile. I know what the word "exec" means. I understand that `rake` maintains all the different scripty things you can do, and I know that `db:migrate` is one of those. I just don't know what all these words are doing together. Why should `bundle` be used to execute `rake` to execute a database migrate?

## Answer
        
[`bundle exec`](http://bundler.io/man/bundle-exec.1.html) is a [Bundler](http://bundler.io) command to execute a script in the context of the current bundle (the one from your directory's [Gemfile](http://bundler.io/gemfile)). `rake db:migrate` is the script where _db_ is the namespace and _migrate_ is the task name defined.

So `bundle exec rake db:migrate` executes the rake script with the command `db:migrate` in the context of the current bundle.

As to the "why?" I'll quote from the [bundler page](http://bundler.io/):

> In some cases, running executables without `bundle exec` may work, if the executable happens to be installed in your system and does not pull in any gems that conflict with your bundle.
> 
> However, this is unreliable and is the source of considerable pain. Even if it looks like it works, it may not work in the future or on another machine.
