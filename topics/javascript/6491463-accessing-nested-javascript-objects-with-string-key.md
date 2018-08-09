
# Accessing nested JavaScript objects with string key

## Question
        
I have a data structure like this :

    var someObject = {
        'part1' : {
            'name': 'Part 1',
            'size': '20',
            'qty' : '50'
        },
        'part2' : {
            'name': 'Part 2',
            'size': '15',
            'qty' : '60'
        },
        'part3' : [
            {
                'name': 'Part 3A',
                'size': '10',
                'qty' : '20'
            }, {
                'name': 'Part 3B',
                'size': '5',
                'qty' : '20'
            }, {
                'name': 'Part 3C',
                'size': '7.5',
                'qty' : '20'
            }
        ]
    };
    

And I would like to access the data using these variable :

    var part1name = "part1.name";
    var part2quantity = "part2.qty";
    var part3name1 = "part3[0].name";
    

part1name should be filled with `someObject.part1.name` 's value, which is "Part 1". Same thing with part2quantity which filled with 60.

Is there anyway to achieve this with either pure javascript or JQuery?

## Answer
        
I just made this based on some similar code I already had, it appears to work:

    Object.byString = function(o, s) {
        s = s.replace(/\[(\w+)\]/g, '.$1'); // convert indexes to properties
        s = s.replace(/^\./, '');           // strip a leading dot
        var a = s.split('.');
        for (var i = 0, n = a.length; i < n; ++i) {
            var k = a[i];
            if (k in o) {
                o = o[k];
            } else {
                return;
            }
        }
        return o;
    }
    

Usage::

    Object.byString(someObj, 'part3[0].name');
    

See a working demo at [http://jsfiddle.net/alnitak/hEsys/](http://jsfiddle.net/alnitak/hEsys/)

**EDIT** some have noticed that this code will throw an error if passed a string where the left-most indexes don't correspond to a correctly nested entry within the object. This is a valid concern, but IMHO best addressed with a `try / catch` block when calling, rather than having this function silently return `undefined` for an invalid index.
