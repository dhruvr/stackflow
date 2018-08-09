
# Connecting Rails 3.1 with Multiple Databases

## Question
        
At ShowNearby we have been doing a very big migration to RoR 3.1 from PHP and we are facing several problems that may be some of you have solved before.

We have big amounts of data and we decided to segregate our DB into several DBs that we can handle separately. For example, our accounts, places, logs and others are split into several databases

We need to get migrations, fixtures, models, to play nicely, and so far it has been quite messy. Some of our requirements for a solution to be acceptable:

*   one model should relate to one tables in one of the databases.
*   rake db:drop - should drop all the database env we specify in database.yml
*   rake db:create - should create all the database env we specify in database.yml
*   rake db:migrate - should run migrations to the various databases
*   rake db:test - should grab fixtures and drop them into the various databases and test unit/function/etc

We are considering setting separate rails projects per each database and connecting them with ActiveResource, but we feel this is not very efficient. Have any of you deal with a similar problem before?

Thanks so much!!

## Answer
        
To Wukerplank's answer, you can also put the connection details in database.yml like usual with a name like so:

    log_database_production:
      adapter: mysql
      host: other_host
      username: logmein
      password: supersecret
      database: logs
    

Then in your special model:

    class AccessLog < ActiveRecord::Base
      establish_connection "log_database_#{Rails.env}".to_sym
    end
    

To keep those pesky credentials from being in your application code.

**Edit:** If you want to reuse this connection in multiple models, you should create a new abstract class and inherit from it, because connections are tightly coupled to classes (as explained [here](https://github.com/rails/rails/issues/7019), [here](http://tenderlovemaking.com/2011/10/20/connection-management-in-activerecord.html), and [here](http://ilikestuffblog.com/2012/09/21/establishing-a-connection-to-a-non-default-database-in-rails-3-2-2/)), and new connections will be created for each class.

If that is the case, set things up like so:

    class LogDatabase < ActiveRecord::Base
      self.abstract_class = true
      establish_connection "log_database_#{Rails.env}".to_sym
    end
    
    class AccessLog < LogDatabase
    end
    
    class CheckoutLog < LogDatabase
    end
