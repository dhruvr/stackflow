
# Sort array of objects by string property value in JavaScript

## Question
        
I have an array of JavaScript objects:

    var objs = [ 
        { first_nom: 'Lazslo', last_nom: 'Jamf'     },
        { first_nom: 'Pig',    last_nom: 'Bodine'   },
        { first_nom: 'Pirate', last_nom: 'Prentice' }
    ];
    

How can I sort them by the value of `last_nom` in JavaScript?

I know about `sort(a,b)`, but that only seems to work on strings and numbers. Do I need to add a toString method to my objects?

## Answer
        
It's easy enough to write your own comparison function:

    function compare(a,b) {
      if (a.last_nom < b.last_nom)
        return -1;
      if (a.last_nom > b.last_nom)
        return 1;
      return 0;
    }
    
    objs.sort(compare);
    

Or inline (c/o Marco Demaio):

    objs.sort(function(a,b) {return (a.last_nom > b.last_nom) ? 1 : ((b.last_nom > a.last_nom) ? -1 : 0);} );
