
# Rails 3 - can&apos;t install pg gem

## Question
        
When I try to run bundle (bundle install), I all the time get

    Installing pg (0.13.2) with native extensions 
    Gem::Installer::ExtensionBuildError: ERROR: Failed to build gem native extension.
    
            /Users/ryan/.rvm/rubies/ruby-1.9.2-p290/bin/ruby extconf.rb 
    checking for pg_config... no
    No pg_config... trying anyway. If building fails, please try again with
     --with-pg-config=/path/to/pg_config
    checking for libpq-fe.h... no
    Can't find the 'libpq-fe.h header
    *** extconf.rb failed ***
    Could not create Makefile due to some reason, probably lack of
    necessary libraries and/or headers.  Check the mkmf.log file for more
    details.  You may need configuration options.
    
    Provided configuration options:
        --with-opt-dir
        --without-opt-dir
        --with-opt-include
        --without-opt-include=${opt-dir}/include
        --with-opt-lib
        --without-opt-lib=${opt-dir}/lib
        --with-make-prog
        --without-make-prog
        --srcdir=.
        --curdir
        --ruby=/Users/ryan/.rvm/rubies/ruby-1.9.2-p290/bin/ruby
        --with-pg
        --without-pg
        --with-pg-dir
        --without-pg-dir
        --with-pg-include
        --without-pg-include=${pg-dir}/include
        --with-pg-lib
        --without-pg-lib=${pg-dir}/lib
        --with-pg-config
        --without-pg-config
        --with-pg_config
        --without-pg_config
    
    
    Gem files will remain installed in /Users/ryan/.rvm/gems/ruby-1.9.2-p290/gems/pg-0.13.2 for inspection.
    Results logged to /Users/ryan/.rvm/gems/ruby-1.9.2-p290/gems/pg-0.13.2/ext/gem_make.out
    An error occured while installing pg (0.13.2), and Bundler cannot continue.
    Make sure that `gem install pg -v '0.13.2'` succeeds before bundling.
    

I use Mac OS X 10.6, the version of installed PostgreSQL is 9.1. I found the problem is in the **libpq-dev**, how could I install/fix that?

## Answer
        
As stated in your error log you need to pass in the path to the pg_config. Try to install the gem using:

    gem install pg -- --with-pg-config= 'PATH_TO_YOUR_PG_CONFIG'
    

If you are not sure where your pg_config is, and assuming you are on Linux or Mac, you can run the following command:

    which pg_config
    

Your pg-config can be in different locations depending on how you installed postgres.
