
# OO Design in Rails: Where to put stuff

## Question
        
I'm really enjoying Rails (even though I'm generally RESTless), and I enjoy Ruby being very OO. Still, the tendency to make huge ActiveRecord subclasses and huge controllers is quite natural (even if you do use a controller per resource). If you were to create deeper object worlds, where would you put the classes (and modules, I suppose)? I'm asking about views (in the Helpers themselves?), controllers and models.

Lib is okay, and I've found [some solutions to get it to reload in a dev environment](http://www.ruby-forum.com/topic/111410), but I'd like to know if there's a better way to do this stuff. I'm really just concerned about classes growing too large. Also, what about Engines and how do they fit in?

## Answer
        
Because Rails provides structure in terms of MVC, it's natural to end up using **only** the model, view, and controller containers that are provided for you. The typical idiom for beginners (and even some intermediate programmers) is to cram all logic in the app into the model (database class), controller, or view.

At some point, someone points out the "fat-model, skinny-controller" paradigm, and intermediate developers hastily excise everything from their controllers and throw it into the model, which starts to become a new trash can for application logic.

Skinny controllers are, in fact, a good idea, but the corollary--putting everything in the model, isn't really the best plan.

In Ruby, you have a couple of good options for making things more modular. A fairly popular answer is to just use modules (usually stashed in `lib`) that hold groups of methods, and then include the modules into the appropriate classes. This helps in cases where you have categories of functionality that you wish to reuse in multiple classes, but where the functionality is still notionally attached to the classes.

Remember, when you include a module into a class, the methods become instance methods of the class, so you still end up with a class containing a **ton** of methods, they're just organized nicely into multiple files.

This solution can work well in some cases--in other cases, you're going to want to think about using classes in your code that are **not** models, views or controllers.

A good way to think about it is the "single responsibility principle," which says that a class should be responsible for a single (or small number) of things. Your models are responsible for persisting data from your application to the database. Your controllers are responsible for receiving a request and returning a viable response.

If you have concepts that don't fit neatly into those boxes (persistence, request/response management), you probably want to think about how you **would** model the idea in question. You can store non-model classes in app/classes, or anywhere else, and add that directory to your load path by doing:

    config.load_paths << File.join(Rails.root, "app", "classes")
    

If you're using passenger or JRuby, you probably also want to add your path to the eager load paths:

    config.eager_load_paths << File.join(Rails.root, "app", "classes")
    

The bottom-line is that once you get to a point in Rails where you find yourself asking this question, it's time to beef up your Ruby chops and start modeling classes that aren't just the MVC classes that Rails gives you by default.

**Update:** This answer applies to Rails 2.x and higher.
