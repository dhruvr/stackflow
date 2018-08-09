
# How to randomize (shuffle) a JavaScript array?

## Question
        
I have an array like this:

    var arr1 = ["a", "b", "c", "d"];
    

How can I randomize / shuffle it?

## Answer
        
The de-facto unbiased shuffle algorithm is the Fisher-Yates (aka Knuth) Shuffle.

See [https://github.com/coolaj86/knuth-shuffle](https://github.com/coolaj86/knuth-shuffle)

You can see a [great visualization here](http://bost.ocks.org/mike/shuffle/) (and the original post [linked to this](http://sedition.com/perl/javascript-fy.html))

    function shuffle(array) {
      var currentIndex = array.length, temporaryValue, randomIndex;
    
      // While there remain elements to shuffle...
      while (0 !== currentIndex) {
    
        // Pick a remaining element...
        randomIndex = Math.floor(Math.random() * currentIndex);
        currentIndex -= 1;
    
        // And swap it with the current element.
        temporaryValue = array[currentIndex];
        array[currentIndex] = array[randomIndex];
        array[randomIndex] = temporaryValue;
      }
    
      return array;
    }
    
    // Used like so
    var arr = [2, 11, 37, 42];
    arr = shuffle(arr);
    console.log(arr);

Some more info [about the algorithm](http://en.wikipedia.org/wiki/Fisher-Yates_shuffle) used.
