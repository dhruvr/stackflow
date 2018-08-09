
# How do I loop through or enumerate a JavaScript object?

## Question
        
I have a JavaScript object like the following:

    var p = {
        "p1": "value1",
        "p2": "value2",
        "p3": "value3"
    };
    

Now I want to loop through all `p` elements (`p1`,`p2`,`p3`...) and get their keys and values. How can I do that?

I can modify the JavaScript object if necessary. My ultimate goal is to loop through some key value pairs and if possible I want to avoid using `eval`.

## Answer
        
You can use the `for-in` loop as shown by others. However, you also have to make sure that the key you get is an actual property of an object, and doesn't come from the prototype.

**Here is the snippet:**

    var p = {
        "p1": "value1",
        "p2": "value2",
        "p3": "value3"
    };
    
    for (var key in p) {
        if (p.hasOwnProperty(key)) {
            console.log(key + " -> " + p[key]);
        }
    }
