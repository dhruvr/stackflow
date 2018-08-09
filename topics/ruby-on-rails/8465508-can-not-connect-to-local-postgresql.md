
# Can not connect to local PostgreSQL

## Question
        
I've managed to bork my local development environment.

All my local Rails apps are now giving the error:

    PGError
    could not connect to server: Permission denied
        Is the server running locally and accepting
        connections on Unix domain socket "/var/pgsql_socket/.s.PGSQL.5432"?
    

I've no idea what's caused this.

While searching for a solution I've updated all bundled gems, updated system gems, updated MacPorts. No joy.

Others have reported this issue when upgrading from OSX Leopard to Lion, due to confusion over which version of Postgres should be used (i.e., OSX version or MacPorts version). I've been running Lion for several months, so it seems strange that this should happen now.

I'm reluctant to mess around too much without first understanding what the problem is. How can I debug this methodically?

How can I determine how many versions of PostgreSQL are on my system, which one is being accessed, and where it is located? How do I fix this if the wrong PostgreSQL is being used?

Sorry for the noob questions. I'm still learning how this works! Thanks for any pointers.

EDIT

Some updates based on suggestions and comments below.

I tried to run `pg_lsclusters` which returned a `command not found` error.

I then tried to local my pg_hba.conf file and found these three sample files:

    /opt/local/share/postgresql84/pg_hba.conf.sample
    /opt/local/var/macports/software/postgresql84/8.4.7_0/opt/local/share/postgresql84/pg_hba.conf.sample
    /usr/share/postgresql/pg_hba.conf.sample
    

So I assume 3 versions of PSQL are installed? Macports, OSX default and ???.

I then did a search for the launchctl startup script `ps -ef | grep postgres` which returned

    0    56     1   0 11:41AM ??         0:00.02 /opt/local/bin/daemondo --label=postgresql84-server --start-cmd /opt/local/etc/LaunchDaemons/org.macports.postgresql84-server/postgresql84-server.wrapper start ; --stop-cmd /opt/local/etc/LaunchDaemons/org.macports.postgresql84-server/postgresql84-server.wrapper stop ; --restart-cmd /opt/local/etc/LaunchDaemons/org.macports.postgresql84-server/postgresql84-server.wrapper restart ; --pid=none
      500   372     1   0 11:42AM ??         0:00.17 /opt/local/lib/postgresql84/bin/postgres -D /opt/local/var/db/postgresql84/defaultdb
      500   766   372   0 11:43AM ??         0:00.37 postgres: writer process                                                                                                                                                                                                                                                                                                                   
      500   767   372   0 11:43AM ??         0:00.24 postgres: wal writer process                                                                                                                                                                                                                                                                                                               
      500   768   372   0 11:43AM ??         0:00.16 postgres: autovacuum launcher process                                                                                                                                                                                                                                                                                                      
      500   769   372   0 11:43AM ??         0:00.08 postgres: stats collector process                                                                                                                                                                                                                                                                                                          
      501  4497  1016   0 12:36PM ttys000    0:00.00 grep postgres
    

I've posted the contents of postgresql84-server.wrapper at [http://pastebin.com/Gj5TpP62](http://pastebin.com/Gj5TpP62).

I tried to run `port load postgresql184-server` but received an error `Error: Port postgresql184-server not found`.

I'm still very confused how to fix this, and appreciate any "for dummies" pointers.

Thanks!

EDIT2

This issue began after I had some problems with daemondo. My local Rails apps were crashing with an application error along the lines of "daemondo gem can not be found". I then went through a series of bundle updates, gem updates, port updates and brew updates to try and find the issue.

Could this error be an issue with daemondo?

## Answer
        
This really looks like a file permissions error. Unix domain sockets are files and have user permissions just like any other. It looks as though the OSX user attempting to access the database does not have file permissions to access the socket file. To confirm this I've done some tests on Ubuntu and psql to try to generate the same error (included below).

You need to check the permissions on the socket file and its directories `/var` and `/var/pgsql_socket`. Your Rails app (OSX user) must have execute (x) permissions on these directories (preferably grant everyone permissions) and the socket should have full permissions (wrx). You can use `ls -lAd <file>` to check these, and if any of them are a symlink you need to check the file or dir the link points to.

You can change the permissions on the dir for youself, but the socket is configured by postgres in `postgresql.conf`. This can be found in the same directory as `pg_hba.conf` (You'll have to figure out which one). Once you've set the permissions you will need to restart postgresql.

    # postgresql.conf should contain...
    unix_socket_directory = '/var/run/postgresql'       # dont worry if yours is different
    #unix_socket_group = ''                             # default is fine here
    #unix_socket_permissions = 0777                     # check this one and uncomment if necessary.
    

* * *

**EDIT:**

I've done a quick search on google which you may wish to look into to see if it is relavent. This might well result in any attempt to `find` your config file failing.

[http://www.postgresqlformac.com/server/howto\_edit\_postgresql_confi.html](http://www.postgresqlformac.com/server/howto_edit_postgresql_confi.html)

* * *

**Error messages:**

User not found in pg_hba.conf

    psql: FATAL:  no pg_hba.conf entry for host "[local]", user "couling", database "main", SSL off
    

User failed password auth:

    psql: FATAL:  password authentication failed for user "couling"
    

Missing unix socket file:

    psql: could not connect to server: No such file or directory
        Is the server running locally and accepting
        connections on Unix domain socket "/var/run/postgresql/.s.PGSQL.5432"?
    

Unix socket exists, but server not listening to it.

    psql: could not connect to server: Connection refused
        Is the server running locally and accepting
        connections on Unix domain socket "/var/run/postgresql/.s.PGSQL.5432"?
    

**Bad file permissions on unix socket file**:

    psql: could not connect to server: Permission denied
        Is the server running locally and accepting
        connections on Unix domain socket "/var/run/postgresql/.s.PGSQL.5432"?
