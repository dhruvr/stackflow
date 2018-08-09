
# Using Rails, how can I set my primary key to not be an integer-typed column?

## Question
        
I'm using Rails migrations to manage a database schema, and I'm creating a simple table where I'd like to use a non-integer value as the primary key (in particular, a string). To abstract away from my problem, let's say there's a table `employees` where employees are identified by an alphanumeric string, e.g. `"134SNW"`.

I've tried creating the table in a migration like this:

    create_table :employees, {:primary_key => :emp_id} do |t|
        t.string :emp_id
        t.string :first_name
        t.string :last_name
    end
    

What this gives me is what seems like it completely ignored the line `t.string :emp_id` and went ahead and made it an integer column. Is there some other way to have rails generate the PRIMARY_KEY constraint (I'm using PostgreSQL) for me, without having to write the SQL in an `execute` call?

**NOTE**: I know it's not best to use string columns as primary keys, so please no answers just saying to add an integer primary key. I may add one anyway, but this question is still valid.

## Answer
        
Unfortunately, I've determined it's not possible to do it without using `execute`.

Why it doesn't work
-------------------

By examining the ActiveRecord source, we can find the code for `create_table`:

In [`schema_statements.rb`](http://github.com/rails/rails/blob/5a7d31913bb16977ec85d55de34ac0d79b512d62/activerecord/lib/active_record/connection_adapters/abstract/schema_statements.rb#L161):

    def create_table(table_name, options={})
      ...
      table_definition.primary_key(options[:primary_key] || Base.get_primary_key(table_name.to_s.singularize)) unless options[:id] == false
      ...
    end
    

So we can see that when you try to specify a primary key in the `create_table` options, it creates a primary key with that specified name (or, if none is specified, `id`). It does this by calling the same method you can use inside a table definition block: `primary_key`.

In [`schema_statements.rb`](https://github.com/rails/rails/blob/a63f7a136401b59db52fb802180f6c9793d83bc2/activerecord/lib/active_record/connection_adapters/abstract/schema_definitions.rb#L89):

    def primary_key(name)
      column(name, :primary_key)
    end
    

This just creates a column with the specified name of type `:primary_key`. This is set to the following in the standard database adapters:

    PostgreSQL: "serial primary key"
    MySQL: "int(11) DEFAULT NULL auto_increment PRIMARY KEY"
    SQLite: "INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL"
    

The workaround
--------------

Since we're stuck with these as the primary key types, we have to use `execute` to create a primary key that is not an integer (PostgreSQL's `serial` is an integer using a sequence):

    create_table :employees, {:id => false} do |t|
      t.string :emp_id
      t.string :first_name
      t.string :last_name
    end
    execute "ALTER TABLE employees ADD PRIMARY KEY (emp_id);"
    

And as [Sean McCleary mentioned](https://stackoverflow.com/questions/1200568/using-rails-how-can-i-set-my-primary-key-to-not-be-an-integer-typed-column/1429146#1429146), your ActiveRecord model should set the primary key using `set_primary_key`:

    class Employee < ActiveRecord::Base
      set_primary_key :emp_id
      ...
    end
