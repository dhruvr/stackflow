
# &apos;sudo gem install&apos; or &apos;gem install&apos; and gem locations

## Question
        
Running '`sudo gem list --local`' and '`gem list --local`' give me differing results. My gem path is set to my home folder and only contains the gems from '`gem list --local`'.

It's probably not good to have gems installed in different directories on my computer, so should I have the gem path set differently, and should I always use `sudo` when installing something?

    my ~/.profile
    export PATH=/opt/local/bin:/opt/local/sbin:$PATH
    export PATH="/usr/local/bin:/usr/local/sbin:/usr/local/mysql/bin:$PATH"
    

~/.bash_profile is empty.

## Answer
        
Contrary to all the other posts I suggest NOT using `sudo` when installing gems.

Instead I recommend you install **[RVM](https://rvm.io/)** and start a happy life with portable gem homes and different version of Ruby all living under one roof.

For the uninitiated, from [the documentation](http://rvm.io):

> RVM is a command line tool which allows us to easily install, manage and work with multiple ruby environments and sets of gems.

The reason why installing gems with `sudo` is worse than just `gem install` is because it installs the gems for _ALL USERS_ as `root`. This might be fine if you're the only person using the machine, but if you're not it can cause weirdness.

If you decide you want to blow away all your gems and start again it's much easier, and safer, to do so as a non-root user.

If you decide you want to use `RVM` then using `sudo` will cause all kinds of weirdness because each Ruby version you install through `RVM` has its own GEM_HOME.

Also, it's nice if you can make your development environment as close to your production environment as possible, and in production you'll most likely install gems as a non-root user.
