
# How to format a JavaScript date

## Question
        
How can I format a JavaScript date object to print as `10-Aug-2010`?

## Answer
        
> Attention: There are better answers below. This answer was written in 2010 and newer and better solutions have arrived since. The OP should accept another answer.

    function formatDate(date) {
      var monthNames = [
        "January", "February", "March",
        "April", "May", "June", "July",
        "August", "September", "October",
        "November", "December"
      ];
    
      var day = date.getDate();
      var monthIndex = date.getMonth();
      var year = date.getFullYear();
    
      return day + ' ' + monthNames[monthIndex] + ' ' + year;
    }
    
    console.log(formatDate(new Date()));  // show current date-time in console

You can edit the array `monthNames` to use Jan, Feb, Mar, etc..
