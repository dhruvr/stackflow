
# Can&apos;t find the PostgreSQL client library (libpq)

## Question
        
I'm trying to install PostgreSQL for Rails on Mac OS X 10.6. First I tried the MacPorts install but that didn't go well so I did the one-click DMG install. That seemed to work.

I suspect I need to install the PostgreSQL development packages but I have no idea how to do that on OS X.

Here's what I get when I try to do `sudo gem install pg`:

    $ sudo gem install pg
    Building native extensions.  This could take a while...
    ERROR:  Error installing pg:
        ERROR: Failed to build gem native extension.
    
            /System/Library/Frameworks/Ruby.framework/Versions/1.8/usr/bin/ruby extconf.rb
    checking for pg_config... yes
    Using config values from /Library/PostgreSQL/8.3/bin/pg_config
    checking for libpq-fe.h... yes
    checking for libpq/libpq-fs.h... yes
    checking for PQconnectdb() in -lpq... no
    checking for PQconnectdb() in -llibpq... no
    checking for PQconnectdb() in -lms/libpq... no
    Can't find the PostgreSQL client library (libpq)
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
        --ruby=/System/Library/Frameworks/Ruby.framework/Versions/1.8/usr/bin/ruby
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
        --with-pqlib
        --without-pqlib
        --with-libpqlib
        --without-libpqlib
        --with-ms/libpqlib
        --without-ms/libpqlib
    
    
    Gem files will remain installed in /Library/Ruby/Gems/1.8/gems/pg-0.11.0 for inspection.
    Results logged to /Library/Ruby/Gems/1.8/gems/pg-0.11.0/ext/gem_make.out

## Answer
        
Solution: reinstalled PostgreSQL with Homebrew.
