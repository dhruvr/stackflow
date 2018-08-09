
# Is there a way to get a collection of all the Models in your Rails app?

## Question
        
Is there a way that you can get a collection of all of the Models in your Rails app?

Basically, can I do the likes of: -

    Models.each do |model|
      puts model.class.name
    end

## Answer
        
**EDIT: Look at the comments and other answers. There are smarter answers than this one! Or try to improve this one as community wiki.**

Models do not register themselves to a master object, so no, Rails does not have the list of models.

But you could still look in the content of the models directory of your application...

    Dir.foreach("#{RAILS_ROOT}/app/models") do |model_path|
      # ...
    end
    

EDIT: Another (wild) idea would be to use Ruby reflection to search for every classes that extends ActiveRecord::Base. Don't know how you can list all the classes though...

EDIT: Just for fun, I found a way to list all classes

    Module.constants.select { |c| (eval c).is_a? Class }
    

EDIT: Finally succeeded in listing all models without looking at directories

    Module.constants.select do |constant_name|
      constant = eval constant_name
      if not constant.nil? and constant.is_a? Class and constant.superclass == ActiveRecord::Base
        constant
      end
    end
    

If you want to handle derived class too, then you will need to test the whole superclass chain. I did it by adding a method to the Class class:

    class Class
      def extend?(klass)
        not superclass.nil? and ( superclass == klass or superclass.extend? klass )
      end
    end
    
    def models 
      Module.constants.select do |constant_name|
        constant = eval constant_name
        if not constant.nil? and constant.is_a? Class and constant.extend? ActiveRecord::Base
        constant
        end
      end
    end
