
# How to disable logging of asset pipeline (sprockets) messages in Rails 3.1?

## Question
        
Sprockets tends to be quite verbose in the (dev) log by default under Rails 3.1 (RC1):

    Started GET "/assets/application.css" for 127.0.0.1 at 2011-06-10 17:30:45 -0400
    Compiled app/assets/stylesheets/application.css.scss  (5ms)  (pid 6303)
    
    
    Started GET "/assets/application.js" for 127.0.0.1 at 2011-06-10 17:30:45 -0400
    Compiled app/assets/stylesheets/default.css.scss  (15ms)  (pid 6303)
    
    ...
    Started GET "/assets/default/header_bg.gif" for 127.0.0.1 at 2011-06-10 17:30:45 -0400
    Served asset /default/header_logo.gif - 304 Not Modified  (7ms)  (pid 6303)
    Served asset /default/header_bg.gif - 304 Not Modified  (0ms)  (pid 6246)
    Served asset /default/footer_bg.gif - 304 Not Modified  (49ms)  (pid 6236)
    ...
    

I'd like to either reduce the level of verbosity or disable it altogether. I'm assuming there is a clean way to disable or reduce the verbosity of the logging by adding a config line in either `environment.rb` or `development.rb` similar to `config.active_record.logger = nil` which silences ActiveRecord SQL statements.

## Answer
        
Place the following code in `config/initializers/quiet_assets.rb`

    if Rails.env.development?
      Rails.application.assets.try(:logger=, Logger.new('/dev/null'))
      Rails::Rack::Logger.class_eval do
        def call_with_quiet_assets(env)
          previous_level = Rails.logger.level
          Rails.logger.level = Logger::ERROR if env['PATH_INFO'] =~ %r{^/assets/}
          call_without_quiet_assets(env)
        ensure
          Rails.logger.level = previous_level
        end
        alias_method_chain :call, :quiet_assets
      end
    end
    

Updated: now works for Rails 3.2 too (previous attempt fixes `before_dispatch` now we're going for the root rack `call` instead)

Update: A proper Rack middleware solution (instead of fragile `alias_method_chain`) from @macournoyer [https://github.com/rails/rails/issues/2639#issuecomment-6591735](https://github.com/rails/rails/issues/2639#issuecomment-6591735)
