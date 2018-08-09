
# What is the easiest way to duplicate an activerecord record?

## Question
        
I want to make a copy of an activerecord record, changing a single field in the process (in addition to the **id**). What is the simplest way to accomplish this?

I realize I could create a new record, and then iterate over each of the fields copying the data field-by-field - but I figured there must be an easier way to do this...

such as:

     @newrecord=Record.copy(:id)  *perhaps?*

## Answer
        
To get a copy, use the clone (or dup for rails 3.1) method:

    # rails < 3.1
    new_record = old_record.clone
    
    #rails >= 3.1
    new_record = old_record.dup
    

Then you can change whichever fields you want.

[ActiveRecord overrides the built-in Object#clone](http://api.rubyonrails.com/classes/ActiveRecord/Base.html#M001363) to give you a new (not saved to the DB) record with an unassigned ID.  
Note that it does not copy associations, so you'll have to do this manually if you need to.

[Rails 3.1 clone is a shallow copy, use dup instead...](https://gist.github.com/994614)
