
# Searching serialized data, using active record

## Question
        
I'm trying to do a simple query of a serialized column, how do you do this?

    serialize :mycode, Array
    
    
    1.9.3p125 :026 > MyModel.find(104).mycode
      MyModel Load (0.6ms)  SELECT `mymodels`.* FROM `mymodels` WHERE `mymodels`.`id` = 104 LIMIT 1
     => [43565, 43402] 
    1.9.3p125 :027 > MyModel.find_all_by_mycode("[43402]")
      MyModel Load (0.7ms)  SELECT `mymodels`.* FROM `mymodels` WHERE `mymodels`.`mycode` = '[43402]'
     => [] 
    1.9.3p125 :028 > MyModel.find_all_by_mycode(43402)
      MyModel Load (1.2ms)  SELECT `mymodels`.* FROM `mymodels` WHERE `mymodels`.`mycode` = 43402
     => [] 
    1.9.3p125 :029 > MyModel.find_all_by_mycode([43565, 43402])
      MyModel Load (1.1ms)  SELECT `mymodels`.* FROM `mymodels` WHERE `mymodels`.`mycode` IN (43565, 43402)
     => []

## Answer
        
Basically, you can't. The downside of #serialize is that you're bypassing your database's native abstractions. You're pretty much limited to loading and saving the data.

That said, one very good way to slow your application to a crawl could be:

    MyModel.all.select { |m| m.mycode.include? 43402 }
    

Moral of the story: don't use #serialize for any data you need to query on.
