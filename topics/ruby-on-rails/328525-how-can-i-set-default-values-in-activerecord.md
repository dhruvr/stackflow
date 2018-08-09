
# How can I set default values in ActiveRecord?

## Question
        
How can I set default value in ActiveRecord?

I see a post from Pratik that describes an ugly, complicated chunk of code: [http://m.onkey.org/2007/7/24/how-to-set-default-values-in-your-model](http://m.onkey.org/2007/7/24/how-to-set-default-values-in-your-model)

    class Item < ActiveRecord::Base  
      def initialize_with_defaults(attrs = nil, &block)
        initialize_without_defaults(attrs) do
          setter = lambda { |key, value| self.send("#{key.to_s}=", value) unless
            !attrs.nil? && attrs.keys.map(&:to_s).include?(key.to_s) }
          setter.call('scheduler_type', 'hotseat')
          yield self if block_given?
        end
      end
      alias_method_chain :initialize, :defaults
    end
    

I have seen the following examples googling around:

      def initialize 
        super
        self.status = ACTIVE unless self.status
      end
    

and

      def after_initialize 
        return unless new_record?
        self.status = ACTIVE
      end
    

I've also seen people put it in their migration, but I'd rather see it defined in the model code.

Is there a canonical way to set default value for fields in ActiveRecord model?

## Answer
        
There are several issues with each of the available methods, but I believe that defining an `after_initialize` callback is the way to go for the following reasons:

1.  `default_scope` will initialize values for new models, but then that will become the scope on which you find the model. If you just want to initialize some numbers to 0 then this is _not_ what you want.
2.  Defining defaults in your migration also works part of the time... As has already been mentioned this will _not_ work when you just call Model.new.
3.  Overriding `initialize` can work, but don't forget to call `super`!
4.  Using a plugin like phusion's is getting a bit ridiculous. This is ruby, do we really need a plugin just to initialize some default values?
5.  Overriding `after_initialize` **is deprecated** as of Rails 3. When I override `after_initialize` in rails 3.0.3 I get the following warning in the console:

> DEPRECATION WARNING: Base#after\_initialize has been deprecated, please use Base.after\_initialize :method instead. (called from /Users/me/myapp/app/models/my_model:15)

Therefore I'd say write an `after_initialize` callback, which lets you default attributes _in addition to_ letting you set defaults on associations like so:

      class Person < ActiveRecord::Base
        has_one :address
        after_initialize :init
    
        def init
          self.number  ||= 0.0           #will set the default value only if it's nil
          self.address ||= build_address #let's you set a default association
        end
      end    
    

Now you have **just one** place to look for initialization of your models. I'm using this method until someone comes up with a better one.

Caveats:

1.  For boolean fields do:
    
    `self.bool_field = true if self.bool_field.nil?`
    
    See Paul Russell's comment on this answer for more details
    
2.  If you're only selecting a subset of columns for a model (ie; using `select` in a query like `Person.select(:firstname, :lastname).all`) you will get a `MissingAttributeError` if your `init` method accesses a column that hasn't been included in the `select` clause. You can guard against this case like so:
    
    `self.number ||= 0.0 if self.has_attribute? :number`
    
    and for a boolean column...
    
    `self.bool_field = true if (self.has_attribute? :bool_value) && self.bool_field.nil?`
    
    Also note that the syntax is different prior to Rails 3.2 (see Cliff Darling's comment below)
