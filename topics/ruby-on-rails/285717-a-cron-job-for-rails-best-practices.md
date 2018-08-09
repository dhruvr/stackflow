
# A cron job for rails: best practices?

## Question
        
What's the best way to run scheduled tasks in a Rails environment? Script/runner? Rake?

## Answer
        
I'm using the rake approach (as supported by [heroku](https://devcenter.heroku.com/articles/scheduler))

With a file called lib/tasks/cron.rake ..

    task :cron => :environment do
      puts "Pulling new requests..."
      EdiListener.process_new_messages
      puts "done."
    end
    

To execute from the command line, this is just "rake cron". This command can then be put on the operating system cron/task scheduler as desired.

**Update** this is quite an old question and answer! Some new info:

*   the heroku cron service I referenced has since been replaced by [Heroku Scheduler](https://devcenter.heroku.com/articles/scheduler)
*   for frequent tasks (esp. where you want to avoid the Rails environment startup cost) my preferred approach is to use system cron to call a script that will either (a) poke a secure/private webhook API to invoke the required task in the background or (b) directly enqueue a task on your queuing system of choice
