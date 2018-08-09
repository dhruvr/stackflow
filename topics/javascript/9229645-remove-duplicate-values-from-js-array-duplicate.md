
# Remove duplicate values from JS array [duplicate]

## Question
        
This question already has an answer here:

*   [Get all unique values in a JavaScript array (remove duplicates)](/questions/1960473/get-all-unique-values-in-a-javascript-array-remove-duplicates) 61 answers

I have a very simple JavaScript array that may or may not contain duplicates.

    var names = ["Mike","Matt","Nancy","Adam","Jenny","Nancy","Carl"];
    

I need to remove the duplicates and put the unique values in a new array.

I could point to all the codes that I've tried but I think it's useless because they don't work. I accept jQuery solutions too.

### Similar question:

*   [Get all non-unique values (i.e.: duplicate/more than one occurrence) in an array](https://stackoverflow.com/questions/840781)

## Answer
        
Quick and dirty using jQuery:

    var names = ["Mike","Matt","Nancy","Adam","Jenny","Nancy","Carl"];
    var uniqueNames = [];
    $.each(names, function(i, el){
        if($.inArray(el, uniqueNames) === -1) uniqueNames.push(el);
    });
