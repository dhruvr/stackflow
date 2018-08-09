
# Want to find records with no associated records in Rails 3

## Question
        
Consider a simple association...

    class Person
       has_many :friends
    end
    
    class Friend
       belongs_to :person
    end
    

What is the cleanest way to get all persons that have NO friends in ARel and/or meta_where?

And then what about a has_many :through version

    class Person
       has_many :contacts
       has_many :friends, :through => :contacts, :uniq => true
    end
    
    class Friend
       has_many :contacts
       has_many :people, :through => :contacts, :uniq => true
    end
    
    class Contact
       belongs_to :friend
       belongs_to :person
    end
    

I really don't want to use counter\_cache - and I from what I've read it doesn't work with has\_many :through

I don't want to pull all the person.friends records and loop through them in Ruby - I want to have a query/scope that I can use with the meta_search gem

I don't mind the performance cost of the queries

And the farther away from actual SQL the better...

## Answer
        
This is still pretty close to SQL, but it should get everyone with no friends in the first case:

    Person.where('id NOT IN (SELECT DISTINCT(person_id) FROM friends)')
