
# do..end vs curly braces for blocks in Ruby

## Question
        
I have a coworker who is actively trying to convince me that I should not use do..end and instead use curly braces for defining multiline blocks in Ruby.

I'm firmly in the camp of only using curly braces for short one-liners and do..end for everything else. But I thought I would reach out to the greater community to get some resolution.

So which is it, and why? (Example of some shoulda code)

    context do
      setup { do_some_setup() }
      should "do somthing" do
        # some more code...
      end
    end
    

or

    context {
      setup { do_some_setup() }
      should("do somthing") {
        # some more code...
      }
    }
    

Personally, just looking at the above answers the question for me, but I wanted to open this up to the greater community.

## Answer
        
The general convention is to use do..end for multi-line blocks and curly braces for single line blocks, but there is also a difference between the two that can be illustrated with this example:

    puts [1,2,3].map{ |k| k+1 }
    2
    3
    4
    => nil
    puts [1,2,3].map do |k| k+1; end
    #<Enumerator:0x0000010a06d140>
    => nil
    

This means that {} has a higher precedence than do..end, so keep that in mind when deciding what you want to use.

P.S: One more example to keep in mind while you develop your preferences.

The following code:

    task :rake => pre_rake_task do
      something
    end
    

really means:

    task(:rake => pre_rake_task){ something }
    

And this code:

    task :rake => pre_rake_task {
      something
    }
    

really means:

    task :rake => (pre_rake_task { something })
    

So to get the actual definition that you want, with curly braces, you must do:

    task(:rake => pre_rake_task) {
      something
    }
    

Maybe using braces for parameters is something you want to do anyways, but if you don't it's probably best to use do..end in these cases to avoid this confusion.
