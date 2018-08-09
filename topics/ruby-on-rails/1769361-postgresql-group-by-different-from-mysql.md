
# PostgreSQL GROUP BY different from MySQL?

## Question
        
I've been migrating some of my MySQL queries to PostgreSQL to use Heroku. Most of my queries work fine, but I keep having a similar recurring error when I use group by:

> ERROR: column "XYZ" must appear in the GROUP BY clause or be used in an aggregate function

Could someone tell me what I'm doing wrong?

  
MySQL which works 100%:

    SELECT `availables`.*
    FROM `availables`
    INNER JOIN `rooms` ON `rooms`.id = `availables`.room_id
    WHERE (rooms.hotel_id = 5056 AND availables.bookdate BETWEEN '2009-11-22' AND '2009-11-24')
    GROUP BY availables.bookdate
    ORDER BY availables.updated_at
    

  
PostgreSQL error:

> ActiveRecord::StatementInvalid: PGError: ERROR: column "availables.id" must appear in the GROUP BY clause or be used in an aggregate function:  
> SELECT "availables".* FROM "availables" INNER JOIN "rooms" ON "rooms".id = "availables".room\_id WHERE (rooms.hotel\_id = 5056 AND availables.bookdate BETWEEN E'2009-10-21' AND E'2009-10-23') GROUP BY availables.bookdate ORDER BY availables.updated_at

  
Ruby code generating the SQL:

    expiration = Available.find(:all,
        :joins => [ :room ],
        :conditions => [ "rooms.hotel_id = ? AND availables.bookdate BETWEEN ? AND ?", hostel_id, date.to_s, (date+days-1).to_s ],
        :group => 'availables.bookdate',
        :order => 'availables.updated_at')  
    

  
Expected Output (from working MySQL query):

+-----+-------+-------+------------+---------+---------------+---------------+
| id  | price | spots | bookdate   | room\_id | created\_at    | updated_at    |
+-----+-------+-------+------------+---------+---------------+---------------+
| 414 | 38.0  | 1     | 2009-11-22 | 1762    | 2009-11-20... | 2009-11-20... |
| 415 | 38.0  | 1     | 2009-11-23 | 1762    | 2009-11-20... | 2009-11-20... |
| 416 | 38.0  | 2     | 2009-11-24 | 1762    | 2009-11-20... | 2009-11-20... |
+-----+-------+-------+------------+---------+---------------+---------------+
3 rows in set

## Answer
        
MySQL's totally non standards compliant `GROUP BY` can be emulated by Postgres' `DISTINCT ON`. Consider this:

### MySQL:

    SELECT a,b,c,d,e FROM table GROUP BY a
    

This delivers 1 row per value of `a` (which one, you don't really know). Well actually you can guess, because MySQL doesn't know about hash aggregates, so it will probably use a sort... but it will only sort on `a`, so the order of the rows could be random. Unless it uses a multicolumn index instead of sorting. Well, anyway, it's not specified by the query.

### Postgres:

    SELECT DISTINCT ON (a) a,b,c,d,e FROM table ORDER BY a,b,c
    

This delivers 1 row per value of `a`, this row will be the first one in the sort according to the `ORDER BY` specified by the query. Simple.

Note that here, it's not an aggregate I'm computing. So `GROUP BY` actually makes no sense. `DISTINCT ON` makes a lot more sense.

Rails is married to MySQL, so I'm not surprised that it generates SQL that doesn't work in Postgres.
