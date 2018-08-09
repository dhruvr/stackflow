
# Sorting an array of JavaScript objects

## Question
        
I read the following objects using Ajax and stored them in an array:

    var homes = [
        {
            "h_id": "3",
            "city": "Dallas",
            "state": "TX",
            "zip": "75201",
            "price": "162500"
        }, {
            "h_id": "4",
            "city": "Bevery Hills",
            "state": "CA",
            "zip": "90210",
            "price": "319250"
        }, {
            "h_id": "5",
            "city": "New York",
            "state": "NY",
            "zip": "00010",
            "price": "962500"
        }
    ];
    

How do I create a function to sort the objects by the `price` property in _ascending_ **or** _descending_ order using only JavaScript?

## Answer
        
Sort homes by price in ascending order:

    homes.sort(function(a, b) {
        return parseFloat(a.price) - parseFloat(b.price);
    });
    

Or after ES6 version:

    homes.sort((a, b) => parseFloat(a.price) - parseFloat(b.price));
    

Some documentation can be found [here](https://developer.mozilla.org/en/docs/Web/JavaScript/Reference/Global_Objects/Array/sort).
