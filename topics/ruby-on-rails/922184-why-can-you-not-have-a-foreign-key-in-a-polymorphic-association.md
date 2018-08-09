
# Why can you not have a foreign key in a polymorphic association?

## Question
        
Why can you not have a foreign key in a polymorphic association, such as the one represented below as a Rails model?

    class Comment < ActiveRecord::Base
      belongs_to :commentable, :polymorphic => true
    end
    
    class Article < ActiveRecord::Base
      has_many :comments, :as => :commentable
    end
    
    class Photo < ActiveRecord::Base
      has_many :comments, :as => :commentable
      #...
    end
    
    class Event < ActiveRecord::Base
      has_many :comments, :as => :commentable
    end

## Answer
        
A foreign key must reference only one parent table. This is fundamental to both SQL syntax, and relational theory.

A Polymorphic Association is when a given column may reference either of two or more parent tables. There's no way you can declare that constraint in SQL.

The Polymorphic Associations design breaks rules of relational database design. I don't recommend using it.

There are several alternatives:

*   **Exclusive Arcs:** Create multiple foreign key columns, each referencing one parent. Enforce that exactly one of these foreign keys can be non-NULL.
    
*   **Reverse the Relationship:** Use three many-to-many tables, each references Comments and a respective parent.
    
*   **Concrete Supertable:** Instead of the implicit "commentable" superclass, create a real table that each of your parent tables references. Then link your Comments to that supertable. Pseudo-rails code would be something like the following (I'm not a Rails user, so treat this as a guideline, not literal code):
    
        class Commentable < ActiveRecord::Base
          has_many :comments
        end
        
        class Comment < ActiveRecord::Base
          belongs_to :commentable
        end
        
        class Article < ActiveRecord::Base
          belongs_to :commentable
        end
        
        class Photo < ActiveRecord::Base
          belongs_to :commentable
        end
        
        class Event < ActiveRecord::Base
          belongs_to :commentable
        end
        
    

I also cover polymorphic associations in my presentation [Practical Object-Oriented Models in SQL](http://www.slideshare.net/billkarwin/practical-object-oriented-models-in-sql), and my book [SQL Antipatterns: Avoiding the Pitfalls of Database Programming](http://pragprog.com/book/bksqla/sql-antipatterns).

* * *

Re your comment: Yes, I do know that there's another column that notes the name of the table that the foreign key supposedly points to. This design is not supported by foreign keys in SQL.

What happens, for instance, if you insert a Comment and name "Video" as the name of the parent table for that `Comment`? No table named "Video" exists. Should the insert be aborted with an error? What constraint is being violated? How does the RDBMS know that this column is supposed to name an existing table? How does it handle case-insensitive table names?

Likewise, if you drop the `Events` table, but you have rows in `Comments` that indicate Events as their parent, what should be the result? Should the drop table be aborted? Should rows in `Comments` be orphaned? Should they change to refer to another existing table such as `Articles`? Do the id values that used to point to `Events` make any sense when pointing to `Articles`?

These dilemmas are all due to the fact that Polymorphic Associations depends on using data (i.e. a string value) to refer to metadata (a table name). This is not supported by SQL. Data and metadata are separate.

* * *

> I'm having a hard time wrapping my head around your "Concrete Supertable" proposal.

*   Define `Commentable` as a real SQL table, not just an adjective in your Rails model definition. No other columns are necessary.
    
        CREATE TABLE Commentable (
          id INT AUTO_INCREMENT PRIMARY KEY
        ) TYPE=InnoDB;
        
    
*   Define the tables `Articles`, `Photos`, and `Events` as "subclasses" of `Commentable`, by making their primary key be also a foreign key referencing `Commentable`.
    
        CREATE TABLE Articles (
          id INT PRIMARY KEY, -- not auto-increment
          FOREIGN KEY (id) REFERENCES Commentable(id)
        ) TYPE=InnoDB;
        
        -- similar for Photos and Events.
        
    
*   Define the `Comments` table with a foreign key to `Commentable`.
    
        CREATE TABLE Comments (
          id INT PRIMARY KEY AUTO_INCREMENT,
          commentable_id INT NOT NULL,
          FOREIGN KEY (commentable_id) REFERENCES Commentable(id)
        ) TYPE=InnoDB;
        
    
*   When you want to create an `Article` (for instance), you must create a new row in `Commentable` too. So too for `Photos` and `Events`.
    
        INSERT INTO Commentable (id) VALUES (DEFAULT); -- generate a new id 1
        INSERT INTO Articles (id, ...) VALUES ( LAST_INSERT_ID(), ... );
        
        INSERT INTO Commentable (id) VALUES (DEFAULT); -- generate a new id 2
        INSERT INTO Photos (id, ...) VALUES ( LAST_INSERT_ID(), ... );
        
        INSERT INTO Commentable (id) VALUES (DEFAULT); -- generate a new id 3
        INSERT INTO Events (id, ...) VALUES ( LAST_INSERT_ID(), ... );
        
    
*   When you want to create a `Comment`, use a value that exists in `Commentable`.
    
        INSERT INTO Comments (id, commentable_id, ...)
        VALUES (DEFAULT, 2, ...);
        
    
*   When you want to query comments of a given `Photo`, do some joins:
    
        SELECT * FROM Photos p JOIN Commentable t ON (p.id = t.id)
        LEFT OUTER JOIN Comments c ON (t.id = c.commentable_id)
        WHERE p.id = 2;
        
    
*   When you have only the id of a comment and you want to find what commentable resource it's a comment for. For this, you may find that it's helpful for the Commentable table to designate which resource it references.
    
        SELECT commentable_id, commentable_type FROM Commentable t
        JOIN Comments c ON (t.id = c.commentable_id)
        WHERE c.id = 42;
        
    
    Then you'd need to run a second query to get data from the respective resource table (Photos, Articles, etc.), after discovering from `commentable_type` which table to join to. You can't do it in the same query, because SQL requires that tables be named explicitly; you can't join to a table determined by data results in the same query.
    

Admittedly, some of these steps break the conventions used by Rails. But the Rails conventions are wrong with respect to proper relational database design.
