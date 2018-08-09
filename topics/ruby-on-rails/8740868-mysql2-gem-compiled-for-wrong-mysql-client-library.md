
# mysql2 gem compiled for wrong mysql client library

## Question
        
When try to connect to the mysql server through my rails application, I get the following error

    D:/Program_Files/Ruby192/lib/ruby/site_ruby/1.9.1/rubygems/custom_require.rb:36:in `require': 
    Incorrect MySQL client library version! This gem was compiled for 6.0.0 but the client library is 5.0.27. (RuntimeError)
    

How can I rectify it?

## Answer
        
I had the same problem as you, or at least the symptom was the same.

Background: I was using Rails 3, the mysql2 gem, and MySQL community server version 5.5.21 (32-bit) installed locally on my Windows machine. I grabbed the client library (`libmysql.dll`) from the MySQL installation and copied it to my ruby installation's `bin` folder.

When I ran `bundle exec rake db:create`, I got the same error message as you and I thought "Hey, how can the client library be outdated when I got it from the latest MySQL release?"

There's a helpful message that is shown when you `gem install mysql2`. Unfortunately, if you install the gem with Bundler, Bundler eats the message. Here it is:

    =========================================================================
    You've installed the binary version of mysql2. It was built using MySQL 
    Connector/C version 6.0.2. It's recommended to use the exact same version
    to avoid potential issues.
    
    At the time of building this gem, the necessary DLL files where available
    in the following download:
    
    http://dev.mysql.com/get/Downloads/Connector-C/mysql-connector-c-noinstall-6.0.2-win32.zip/from/pick
    
    And put lib\libmysql.dll file in your Ruby bin directory, for example
    C:\Ruby\bin
    

Following these instructions solved the problem for me.

[Referenced link](http://dev.mysql.com/get/Downloads/Connector-C/mysql-connector-c-noinstall-6.0.2-win32.zip/from/pick)
