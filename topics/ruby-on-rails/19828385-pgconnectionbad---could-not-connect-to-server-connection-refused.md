
# PG::ConnectionBad - could not connect to server: Connection refused

## Question
        
Everytime I run my rails 4.0 server, I get this output.

    Started GET "/" for 127.0.0.1 at 2013-11-06 23:56:36 -0500
    
    PG::ConnectionBad - could not connect to server: Connection refused
    Is the server running on host "localhost" (::1) and accepting
    TCP/IP connections on port 5432?
    could not connect to server: Connection refused
    Is the server running on host "localhost" (127.0.0.1) and accepting
    TCP/IP connections on port 5432?
    could not connect to server: Connection refused
    Is the server running on host "localhost" (fe80::1) and accepting
    TCP/IP connections on port 5432?
    :
     activerecord (4.0.0) lib/active_record/connection_adapters/postgresql_adapter.rb:825:in `connect'
     activerecord (4.0.0) lib/active_record/connection_adapters/postgresql_adapter.rb:542:in `initialize'
     activerecord (4.0.0) lib/active_record/connection_adapters/postgresql_adapter.rb:41:in `postgresql_connection'
     activerecord (4.0.0) lib/active_record/connection_adapters/abstract/connection_pool.rb:440:in `new_connection'
     activerecord (4.0.0) lib/active_record/connection_adapters/abstract/connection_pool.rb:450:in `checkout_new_connection'
     activerecord (4.0.0) lib/active_record/connection_adapters/abstract/connection_pool.rb:421:in `acquire_connection'
     activerecord (4.0.0) lib/active_record/connection_adapters/abstract/connection_pool.rb:356:in `block in checkout'
     /System/Library/Frameworks/Ruby.framework/Versions/2.0/usr/lib/ruby/2.0.0/monitor.rb:211:in `mon_synchronize'
     activerecord (4.0.0) lib/active_record/connection_adapters/abstract/connection_pool.rb:355:in `checkout'
     activerecord (4.0.0) lib/active_record/connection_adapters/abstract/connection_pool.rb:265:in `block in connection'
     /System/Library/Frameworks/Ruby.framework/Versions/2.0/usr/lib/ruby/2.0.0/monitor.rb:211:in `mon_synchronize'
     activerecord (4.0.0) lib/active_record/connection_adapters/abstract/connection_pool.rb:264:in `connection'
     activerecord (4.0.0) lib/active_record/connection_adapters/abstract/connection_pool.rb:546:in `retrieve_connection'
     activerecord (4.0.0) lib/active_record/connection_handling.rb:79:in `retrieve_connection'
     activerecord (4.0.0) lib/active_record/connection_handling.rb:53:in `connection'
     activerecord (4.0.0) lib/active_record/migration.rb:792:in `current_version'
     activerecord (4.0.0) lib/active_record/migration.rb:800:in `needs_migration?'
     activerecord (4.0.0) lib/active_record/migration.rb:379:in `check_pending!'
     activerecord (4.0.0) lib/active_record/migration.rb:366:in `call'
     actionpack (4.0.0) lib/action_dispatch/middleware/callbacks.rb:29:in `block in call'
     activesupport (4.0.0) lib/active_support/callbacks.rb:373:in `_run__1613334440513032208__call__callbacks'
     activesupport (4.0.0) lib/active_support/callbacks.rb:80:in `run_callbacks'
     actionpack (4.0.0) lib/action_dispatch/middleware/callbacks.rb:27:in `call'
     actionpack (4.0.0) lib/action_dispatch/middleware/reloader.rb:64:in `call'
     actionpack (4.0.0) lib/action_dispatch/middleware/remote_ip.rb:76:in `call'
     better_errors (0.9.0) lib/better_errors/middleware.rb:84:in `protected_app_call'
     better_errors (0.9.0) lib/better_errors/middleware.rb:79:in `better_errors_call'
     better_errors (0.9.0) lib/better_errors/middleware.rb:56:in `call'
     actionpack (4.0.0) lib/action_dispatch/middleware/debug_exceptions.rb:17:in `call'
     actionpack (4.0.0) lib/action_dispatch/middleware/show_exceptions.rb:30:in `call'
     railties (4.0.0) lib/rails/rack/logger.rb:38:in `call_app'
     railties (4.0.0) lib/rails/rack/logger.rb:21:in `block in call'
     activesupport (4.0.0) lib/active_support/tagged_logging.rb:67:in `block in tagged'
     activesupport (4.0.0) lib/active_support/tagged_logging.rb:25:in `tagged'
     activesupport (4.0.0) lib/active_support/tagged_logging.rb:67:in `tagged'
     railties (4.0.0) lib/rails/rack/logger.rb:21:in `call'
     quiet_assets (1.0.2) lib/quiet_assets.rb:18:in `call_with_quiet_assets'
     actionpack (4.0.0) lib/action_dispatch/middleware/request_id.rb:21:in `call'
     rack (1.5.2) lib/rack/methodoverride.rb:21:in `call'
     rack (1.5.2) lib/rack/runtime.rb:17:in `call'
     activesupport (4.0.0) lib/active_support/cache/strategy/local_cache.rb:83:in `call'
     rack (1.5.2) lib/rack/lock.rb:17:in `call'
     actionpack (4.0.0) lib/action_dispatch/middleware/static.rb:64:in `call'
     railties (4.0.0) lib/rails/engine.rb:511:in `call'
     railties (4.0.0) lib/rails/application.rb:97:in `call'
     rack (1.5.2) lib/rack/content_length.rb:14:in `call'
     thin (1.5.1) lib/thin/connection.rb:81:in `block in pre_process'
     thin (1.5.1) lib/thin/connection.rb:79:in `pre_process'
     thin (1.5.1) lib/thin/connection.rb:54:in `process'
     thin (1.5.1) lib/thin/connection.rb:39:in `receive_data'
     eventmachine (1.0.3) lib/eventmachine.rb:187:in `run'
     thin (1.5.1) lib/thin/backends/base.rb:63:in `start'
     thin (1.5.1) lib/thin/server.rb:159:in `start'
     rack (1.5.2) lib/rack/handler/thin.rb:16:in `run'
     rack (1.5.2) lib/rack/server.rb:264:in `start'
     railties (4.0.0) lib/rails/commands/server.rb:84:in `start'
     railties (4.0.0) lib/rails/commands.rb:78:in `block in <top (required)>'
     railties (4.0.0) lib/rails/commands.rb:73:in `<top (required)>'
     bin/rails:4:in `<main>'
    

I'm running Mavericks OS X 10.9 so I don't know if that's the problem. I've tried everything I could but nothing seems to work. I've uninstalled and install both postgres and the pg gem multiple times now.

This is my database.yml file

    development:
      adapter: postgresql
      encoding: unicode
      database: metals-directory_development
      pool: 5
      username: 
      password: 
      template: template0
      host: localhost
      port: 5432
    
    test: &test
      adapter: postgresql
      encoding: unicode
      database: metals-directory_test
      pool: 5
      username: 
      password: 
      template: template0
      host: localhost
      port: 5432
    
    staging:
      adapter: postgresql
      encoding: unicode
      database: metals-directory_production
      pool: 5
      username:
      password:
      template: template0
      host: localhost
    
    production:
      adapter: postgresql
      encoding: unicode
      database: metals-directory_production
      pool: 5
      username:
      password:
      template: template0
      host: localhost
    
    cucumber:
      <<: *test
    

Can anyone help me?

## Answer
        
It could be as simple as a stale **PID file**. It could be failing silently because your computer didn't complete the shutdown process completely which means **postgres** didn't delete the **PID** (process id) file.

The PID file is used by postgres to make sure only one instance of the server is running at a time. So when it goes to start again, it fails because there is already a **PID** file which tells **postgres** that another instance of the server was started (even though it isn't running, it just didn't get to shutdown and delete the PID).

1.  To fix it remove/rename the PID file. Find the postgres data directory. On a MAC using homebrew it is `/usr/local/var/postgres/`, other systems it might be `/usr/var/postgres/`.
2.  To make sure this is the problem, look at the log file (`server.log`). On the last lines you will see:

> FATAL: lock file "postmaster.pid" already exists  
> HINT: Is another postmaster (PID 347) running in data directory "/usr/local/var/postgres"?

3.  If so, `rm postmaster.pid`
4.  Restart your server. On a mac using launchctl (with homebrew) the following commands will restart the server.
    
        launchctl unload homebrew.mxcl.postgresql.plist  
        launchctl load -w homebrew.mxcl.postgresql.plist
        
    
    OR on newer versions of **Brew**
    
        brew services restart postgresql
