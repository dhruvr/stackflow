
# How to compare arrays in JavaScript?

## Question
        
I'd like to compare two arrays... ideally, efficiently. Nothing fancy, just `true` if they are identical, and `false` if not. Not surprisingly, the comparison operator doesn't seem to work.

    var a1 = [1,2,3];
    var a2 = [1,2,3];
    console.log(a1==a2);    // Returns false
    console.log(JSON.stringify(a1)==JSON.stringify(a2));    // Returns true
    

JSON encoding each array does, but is there a faster or "better" way to simply compare arrays without having to iterate through each value?

## Answer
        
To compare arrays, loop through them and compare every value:

Comparing arrays:
-----------------

    // Warn if overriding existing method
    if(Array.prototype.equals)
        console.warn("Overriding existing Array.prototype.equals. Possible causes: New API defines the method, there's a framework conflict or you've got double inclusions in your code.");
    // attach the .equals method to Array's prototype to call it on any array
    Array.prototype.equals = function (array) {
        // if the other array is a falsy value, return
        if (!array)
            return false;
    
        // compare lengths - can save a lot of time 
        if (this.length != array.length)
            return false;
    
        for (var i = 0, l=this.length; i < l; i++) {
            // Check if we have nested arrays
            if (this[i] instanceof Array && array[i] instanceof Array) {
                // recurse into the nested arrays
                if (!this[i].equals(array[i]))
                    return false;       
            }           
            else if (this[i] != array[i]) { 
                // Warning - two different object instances will never be equal: {x:20} != {x:20}
                return false;   
            }           
        }       
        return true;
    }
    // Hide method from for-in loops
    Object.defineProperty(Array.prototype, "equals", {enumerable: false});
    

### Usage:

    [1, 2, [3, 4]].equals([1, 2, [3, 2]]) === false;
    [1, "2,3"].equals([1, 2, 3]) === false;
    [1, 2, [3, 4]].equals([1, 2, [3, 4]]) === true;
    [1, 2, 1, 2].equals([1, 2, 1, 2]) === true;
    

You may say "_But it is much faster to compare strings - no loops..._" well, then you should note there ARE loops. First recursive loop that converts Array to string and second, that compares two strings. So this method **is faster than use of string**.

I believe that larger amounts of data should be always stored in arrays, not in objects. However if you use objects, they can be partially compared too.  
**Here's how:**

Comparing objects:
------------------

I've stated above, that two object **instances** will never be equal, even if they contain same data at the moment:

    ({a:1, foo:"bar", numberOfTheBeast: 666}) == ({a:1, foo:"bar", numberOfTheBeast: 666})  //false
    

This has a reason, since there may be, for example [private variables within objects.](https://stackoverflow.com/a/201471/607407)

However, if you just use object structure to contain data, comparing is still possible:

    Object.prototype.equals = function(object2) {
        //For the first loop, we only check for types
        for (propName in this) {
            //Check for inherited methods and properties - like .equals itself
            //https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Object/hasOwnProperty
            //Return false if the return value is different
            if (this.hasOwnProperty(propName) != object2.hasOwnProperty(propName)) {
                return false;
            }
            //Check instance type
            else if (typeof this[propName] != typeof object2[propName]) {
                //Different types => not equal
                return false;
            }
        }
        //Now a deeper check using other objects property names
        for(propName in object2) {
            //We must check instances anyway, there may be a property that only exists in object2
                //I wonder, if remembering the checked values from the first loop would be faster or not 
            if (this.hasOwnProperty(propName) != object2.hasOwnProperty(propName)) {
                return false;
            }
            else if (typeof this[propName] != typeof object2[propName]) {
                return false;
            }
            //If the property is inherited, do not check any more (it must be equa if both objects inherit it)
            if(!this.hasOwnProperty(propName))
              continue;
    
            //Now the detail check and recursion
    
            //This returns the script back to the array comparing
            /**REQUIRES Array.equals**/
            if (this[propName] instanceof Array && object2[propName] instanceof Array) {
                       // recurse into the nested arrays
               if (!this[propName].equals(object2[propName]))
                            return false;
            }
            else if (this[propName] instanceof Object && object2[propName] instanceof Object) {
                       // recurse into another objects
                       //console.log("Recursing to compare ", this[propName],"with",object2[propName], " both named \""+propName+"\"");
               if (!this[propName].equals(object2[propName]))
                            return false;
            }
            //Normal value comparison for strings and numbers
            else if(this[propName] != object2[propName]) {
               return false;
            }
        }
        //If everything passed, let's say YES
        return true;
    }  
    

However, remember that this one is to serve in comparing JSON like data, not class instances and other stuff. If you want to compare mor complicated objects, look at [this answer and it's superlong function](https://stackoverflow.com/a/1144249/607407).  
To make this work with `Array.equals` you must edit the original function a little bit:

    ...
        // Check if we have nested arrays
        if (this[i] instanceof Array && array[i] instanceof Array) {
            // recurse into the nested arrays
            if (!this[i].equals(array[i]))
                return false;
        }
        /**REQUIRES OBJECT COMPARE**/
        else if (this[i] instanceof Object && array[i] instanceof Object) {
            // recurse into another objects
            //console.log("Recursing to compare ", this[propName],"with",object2[propName], " both named \""+propName+"\"");
            if (!this[i].equals(array[i]))
                return false;
            }
        else if (this[i] != array[i]) {
    ...
    

I made a [little test tool for both of the functions](http://jsfiddle.net/Darker/SBtu2/).

Bonus: Nested arrays with `indexOf` and `contains`
--------------------------------------------------

[Samy Bencherif has prepared](https://stackoverflow.com/questions/7837456/how-to-compare-arrays-in-javascript/14853974?noredirect=1#comment59337127_14853974) useful functions for the case you're searching for a specific object in nested arrays, which are available here: [https://jsfiddle.net/SamyBencherif/8352y6yw/](https://jsfiddle.net/SamyBencherif/8352y6yw/)
