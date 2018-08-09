
# Where can I find documentation on formatting a date in JavaScript? [closed]

## Question
        
I noticed that JavaScript's `new Date()` function is very smart in accepting dates in several formats.

    Xmas95 = new Date("25 Dec, 1995 23:15:00")
    Xmas95 = new Date("2009 06 12,12:52:39")
    Xmas95 = new Date("20 09 2006,12:52:39")
    

I could not find documentation anywhere showing all the valid string formats while calling `new Date()` function.

This is for converting a string to a date. If we look at the opposite side, that is, converting a date object to a string, until now I was under the impression that JavaScript doesn't have a built-in API to format a date object into a string.

> **Editor's note:** The following approach is the asker's attempt that worked on a particular browser but does _not_ work in general; **see the answers on this page** to see some actual solutions.

Today, I played with the `toString()` method on the date object and surprisingly it serves the purpose of formatting date to strings.

    var d1 = new Date();
    d1.toString('yyyy-MM-dd');       //Returns "2009-06-29" in Internet Explorer, but not Firefox or Chrome
    d1.toString('dddd, MMMM ,yyyy')  //Returns "Monday, June 29,2009" in Internet Explorer, but not Firefox or Chrome
    

Also here I couldn't find any documentation on all the ways we can format the date object into a string.

Where is the documentation which lists the format specifiers supported by the `Date()` object?

## Answer
        
I love _[10 ways to format time and date using JavaScript](http://www.webdevelopersnotes.com/tips/html/10_ways_to_format_time_and_date_using_javascript.php3)_ and _[Working with Dates](http://www.elated.com/articles/working-with-dates/)_.

Basically, you have three methods and you have to combine the strings for yourself:

    getDate() // Returns the date
    getMonth() // Returns the month
    getFullYear() // Returns the year
    

Example:

    var d = new Date();
    var curr_date = d.getDate();
    var curr_month = d.getMonth() + 1; //Months are zero based
    var curr_year = d.getFullYear();
    console.log(curr_date + "-" + curr_month + "-" + curr_year);
