
# Why does Date.parse give incorrect results?

## Question
        
### Case One:

    new Date(Date.parse("Jul 8, 2005"));
    

### Output:

Fri Jul 08 2005 00:00:00 GMT-0700 (PST)

### Case Two:

    new Date(Date.parse("2005-07-08"));
    

### Output:

Thu Jul 07 2005 17:00:00 GMT-0700 (PST)

* * *

Why is the second parse incorrect?

## Answer
        
Until the 5th edition spec came out, the [`Date.parse`](http://bclary.com/2004/11/07/#a-15.9.4.2) method was completely _implementation dependent_ (`new Date(string)` is equivalent to [`Date.parse(string)`](http://bclary.com/2004/11/07/#a-15.9.4.2) except the latter returns a number rather than a `Date`). In the 5th edition spec the requirement was added to support a [simplified _(and slightly incorrect)_ ISO-8601](http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.1.15), but other than that, there was _no_ requirement for what `Date.parse` / `new Date(string)` should accept other than that they had to accept whatever _Date#toString_ output (without saying what that was).

As of ECMAScript 2017 (edition 8), implementations were required to parse their output for _Date#toString_ and _Date#toUTCString_, but the format of those strings was not specified.

As of ECMAScript 2019 (edition 9) the format for [_Date#toString_](http://ecma-international.org/ecma-262/9.0/#sec-date.prototype.tostring) and [_Date#toUTCString_](http://ecma-international.org/ecma-262/9.0/#sec-date.prototype.toutcstring), have been specified as (respectively):

1.  ddd MMM DD YYYY HH:mm:ss ZZ \[(timezone name)\]  
    e.g. Tue Jul 10 2018 18:39:58 GMT+0530 (IST)
2.  ddd, DD MMM YYYY HH:mm:ss Z  
    e.g. Tue 10 Jul 2018 13:09:58 GMT

providing 2 more formats that _Date.parse_ should parse reliably in new implementations (noting that support is not ubiquitous and non–compliant implementations will remain in use for some time).

I would recommend that date strings are parsed manually and the [Date constructor](http://bclary.com/2004/11/07/#a-15.9.3.1) used with year, month and day arguments to avoid ambiguity:

    // parse a date in yyyy-mm-dd format
    function parseDate(input) {
      var parts = input.split('-');
      // new Date(year, month [, day [, hours[, minutes[, seconds[, ms]]]]])
      return new Date(parts[0], parts[1]-1, parts[2]); // Note: months are 0-based
    }
