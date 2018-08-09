
# Unable to install gem - Failed to build gem native extension - cannot load such file &#x2014; mkmf (LoadError)

## Question
        
Ruby 1.9.3

The part of Gemfile

    #...............
    gem "pony"
    gem "bcrypt-ruby", :require => "bcrypt"
    gem "nokogiri" 
    #..................
    

When I'm trying to install gems, I get an error

    alex@ubuntu:~/$ bundle
    Fetching gem metadata from http://rubygems.org/.........
    Fetching gem metadata from http://rubygems.org/..
    Enter your password to install the bundled RubyGems to your system: 
    #####............................................................
    Installing bcrypt-ruby (3.0.1) with native extensions 
    Gem::Installer::ExtensionBuildError: ERROR: Failed to build gem native extension.
    
            /usr/bin/ruby1.9.1 extconf.rb 
    /usr/lib/ruby/1.9.1/rubygems/custom_require.rb:36:in `require': cannot load such file -- mkmf (LoadError)
            from /usr/lib/ruby/1.9.1/rubygems/custom_require.rb:36:in `require'
            from extconf.rb:36:in `<main>'
    
    
    Gem files will remain installed in /home/alex/.bundler/tmp/5526/gems/bcrypt-ruby-3.0.1 for inspection.
    Results logged to /home/alex/.bundler/tmp/5526/gems/bcrypt-ruby-3.0.1/ext/mri/gem_make.out
    An error occurred while installing bcrypt-ruby (3.0.1), and Bundler cannot continue.
    Make sure that `gem install bcrypt-ruby -v '3.0.1'` succeeds before bundling.
    

Then I'm doing this

    sudo gem install bcrypt-ruby -v '3.0.1'
    Building native extensions.  This could take a while...
    ERROR:  Error installing bcrypt-ruby:
            ERROR: Failed to build gem native extension.
    
            /usr/bin/ruby1.9.1 extconf.rb
    /usr/lib/ruby/1.9.1/rubygems/custom_require.rb:36:in `require': cannot load such file -- mkmf (LoadError)
            from /usr/lib/ruby/1.9.1/rubygems/custom_require.rb:36:in `require'
            from extconf.rb:36:in `<main>'
    
    
    Gem files will remain installed in /var/lib/gems/1.9.1/gems/bcrypt-ruby-3.0.1 for inspection.
    Results logged to /var/lib/gems/1.9.1/gems/bcrypt-ruby-3.0.1/ext/mri/gem_make.out
    

and getting an error as well.

What did I miss?

## Answer
        
There are similar questions:

*   [`require': no such file to load -- mkmf (LoadError)](https://stackoverflow.com/questions/7645918/require-no-such-file-to-load-mkmf-loaderror)
*   [Failed to build gem native extension (mkmf (LoadError)) - Ubuntu 12.04](https://stackoverflow.com/questions/18316667/failed-to-build-gem-native-extension-mkmf-loaderror-ubuntu-12-04/18316692#18316692)

Usually, the solution is:

> sudo apt-get install ruby-dev

**Or, if that doesn't work, depending on your ruby version**, run something like:

> sudo apt-get install ruby1.9.1-dev

Should fix your problem.

* * *

Still not working? Try the following after installing ruby-dev:

> sudo apt-get install make
