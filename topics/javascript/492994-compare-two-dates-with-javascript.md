
# Compare two dates with JavaScript

## Question
        
Can someone suggest a way to compare the values of **two dates** greater than, less than, and not in the past using JavaScript? The values will be coming from text boxes...

## Answer
        
The [Date object](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date) will do what you want - construct one for each date, then compare them using the `>`, `<`, `<=` or `>=`.

The `==`, `!=`, `===`, and `!==` operators require you to use `date.getTime()` as in

    var d1 = new Date();
    var d2 = new Date(d1);
    var same = d1.getTime() === d2.getTime();
    var notSame = d1.getTime() !== d2.getTime();
    

to be clear just checking for equality directly with the data objects won't work

    var d1 = new Date();
    var d2 = new Date(d1);
    
    console.log(d1 == d2);   // prints false (wrong!) 
    console.log(d1 === d2);  // prints false (wrong!)
    console.log(d1 != d2);   // prints true  (wrong!)
    console.log(d1 !== d2);  // prints true  (wrong!)
    console.log(d1.getTime() === d2.getTime()); // prints true (correct)
    

I suggest you use drop-downs or some similar constrained form of date entry rather than text boxes, though, lest you find yourself in input validation hell.
