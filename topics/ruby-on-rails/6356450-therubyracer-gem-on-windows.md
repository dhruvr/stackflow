
# therubyracer gem on windows

## Question
        
I've been peacefully developing on Windows without adding any gems for a few weeks now and today I decided to do a bundle update, but I cannot get through this gem called therubyracer. I have the devkit installed and it is working according to the documentation's verification procedure.

My question is: is there a way to install this gem at all on windows?

And is this gem going to be required by rails 3.1 and this is why now that I do a bundle update it is being 'slipped' into the rails 3.0.8 as a gesture of early kick start for future 3.1 migration?

EDIT including Gemfile and Gemfile.lock
=======================================

    # Gemfile
    # source 'http://rubygems.org'
    source :rubygems
    
    gem 'rails'
    gem 'rake', '0.8.7'
    gem 'youtube_it'
    gem 'panda'
    gem "nifty-generators"
    # gem "mongoid", "2.0.0.rc.7"
    gem "mongoid"
    gem "mongoid-eager-loading"
    # gem 'mongoid_search'
    gem "bson_ext", ">1.1.5"
    gem 'devise'
    gem 'cancan'
    gem 'hirb'
    # gem 'heroku'
    gem 'rest-client'
    gem 'less' # needs the more plugin
    # gem 'hash_extension'
    gem 'aws-s3', :require => 'aws/s3' # s3.rb
    gem 'jquery-rails', ">= 0.2.7" # rails g jquery:install
    # gem 'mongrel', ">= 1.2.0.pre2"
    gem 'delayed_job'
    gem 'delayed_job_mongoid'
    gem 'kaminari'
    
    
    
    # Gemfile.lock
    GEM
      remote: http://rubygems.org/
      specs:
        abstract (1.0.0)
        actionmailer (3.0.8)
          actionpack (= 3.0.8)
          mail (~> 2.2.19)
        actionpack (3.0.8)
          activemodel (= 3.0.8)
          activesupport (= 3.0.8)
          builder (~> 2.1.2)
          erubis (~> 2.6.6)
          i18n (~> 0.5.0)
          rack (~> 1.2.1)
          rack-mount (~> 0.6.14)
          rack-test (~> 0.5.7)
          tzinfo (~> 0.3.23)
        activemodel (3.0.8)
          activesupport (= 3.0.8)
          builder (~> 2.1.2)
          i18n (~> 0.5.0)
        activerecord (3.0.8)
          activemodel (= 3.0.8)
          activesupport (= 3.0.8)
          arel (~> 2.0.10)
          tzinfo (~> 0.3.23)
        activeresource (3.0.8)
          activemodel (= 3.0.8)
          activesupport (= 3.0.8)
        activesupport (3.0.8)
        arel (2.0.10)
        aws-s3 (0.6.2)
          builder
          mime-types
          xml-simple
        bcrypt-ruby (2.1.4-x86-mingw32)
        bson (1.3.1)
        bson_ext (1.3.1)
        builder (2.1.2)
        cancan (1.6.5)
        daemons (1.1.3)
        delayed_job (2.1.4)
          activesupport (~> 3.0)
          daemons
        delayed_job_mongoid (1.0.2)
          delayed_job (~> 2.1.1)
          mongoid (~> 2.0.0.rc)
        devise (1.3.4)
          bcrypt-ruby (~> 2.1.2)
          orm_adapter (~> 0.0.3)
          warden (~> 1.0.3)
        erubis (2.6.6)
          abstract (>= 1.0.0)
        hirb (0.4.5)
        i18n (0.5.0)
        jquery-rails (1.0.10)
          railties (~> 3.0)
          thor (~> 0.14)
        json (1.5.2)
        kaminari (0.12.4)
          rails (>= 3.0.0)
        less (1.2.21)
          mutter (>= 0.4.2)
          treetop (>= 1.4.2)
        mail (2.2.19)
          activesupport (>= 2.3.6)
          i18n (>= 0.4.0)
          mime-types (~> 1.16)
          treetop (~> 1.4.8)
        mime-types (1.16)
        mongo (1.3.1)
          bson (>= 1.3.1)
        mongoid (2.0.2)
          activemodel (~> 3.0)
          mongo (~> 1.3)
          tzinfo (~> 0.3.22)
        mongoid-eager-loading (0.3.1)
        mutter (0.5.3)
        nifty-generators (0.4.6)
        oauth (0.4.4)
        orm_adapter (0.0.5)
        panda (1.4.2)
          json
          rest-client
          ruby-hmac (>= 0.3.2)
        polyglot (0.3.1)
        rack (1.2.3)
        rack-mount (0.6.14)
          rack (>= 1.0.0)
        rack-test (0.5.7)
          rack (>= 1.0)
        rails (3.0.8)
          actionmailer (= 3.0.8)
          actionpack (= 3.0.8)
          activerecord (= 3.0.8)
          activeresource (= 3.0.8)
          activesupport (= 3.0.8)
          bundler (~> 1.0)
          railties (= 3.0.8)
        railties (3.0.8)
          actionpack (= 3.0.8)
          activesupport (= 3.0.8)
          rake (>= 0.8.7)
          thor (~> 0.14.4)
        rake (0.8.7)
        rest-client (1.6.1)
          mime-types (>= 1.16)
        ruby-hmac (0.4.0)
        thor (0.14.6)
        treetop (1.4.9)
          polyglot (>= 0.3.1)
        tzinfo (0.3.28)
        warden (1.0.4)
          rack (>= 1.0)
        xml-simple (1.0.16)
        youtube_it (1.4.2)
          builder
          oauth (>= 0.4.4)
    
    PLATFORMS
      x86-mingw32
    
    DEPENDENCIES
      aws-s3
      bson_ext (> 1.1.5)
      cancan
      delayed_job
      delayed_job_mongoid
      devise
      hirb
      jquery-rails (>= 0.2.7)
      kaminari
      less
      mongoid
      mongoid-eager-loading
      nifty-generators
      panda
      rails
      rake (= 0.8.7)
      rest-client
      youtube_it

## Answer
        
No, there is no way that I know of to currently install this gem on windows. The problem is that there is currently no pre-compiled version of the libv8 gem for Windows, and the source version is only compatible with *nix. It does not necessarily have to be that way, it just requires somebody taking the time to make the compile work for Windows. [https://github.com/fractaloop/libv8](https://github.com/fractaloop/libv8)

That said, Windows should come with a JScript, the Microsoft JavaScript runtime, which rails (via [execjs](https://github.com/sstephenson/execjs)) will automatically detect and use, so you should be able to just remove your dependency on therubyracer.

As the maintainer of that gem, this would of course make me sad, but it should get you on your way.
