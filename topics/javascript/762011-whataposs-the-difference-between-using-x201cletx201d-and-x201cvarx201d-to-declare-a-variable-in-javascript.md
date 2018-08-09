
# What&apos;s the difference between using &#x201C;let&#x201D; and &#x201C;var&#x201D; to declare a variable in JavaScript?

## Question
        
ECMAScript 6 introduced [the `let` statement](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/let). I've heard it described as a "local" variable, but I'm still not quite sure how it behaves differently than the `var` keyword.

What are the differences? When should `let` be used over `var`?

## Answer
        
The difference is scoping. `var` is scoped to the nearest function block and `let` is scoped to the nearest _enclosing_ block, which can be smaller than a function block. Both are global if outside any block.

Also, variables declared with `let` are not accessible before they are declared in their enclosing block. As seen in the demo, this will throw a ReferenceError exception.

**[Demo](http://jsfiddle.net/tcCp5/182/):**

    var html = '';
    
    write('#### global ####\n');
    write('globalVar: ' + globalVar); //undefined, but visible
    
    try {
      write('globalLet: ' + globalLet); //undefined, *not* visible
    } catch (exception) {
      write('globalLet: exception');
    }
    
    write('\nset variables');
    
    var globalVar = 'globalVar';
    let globalLet = 'globalLet';
    
    write('\nglobalVar: ' + globalVar);
    write('globalLet: ' + globalLet);
    
    function functionScoped() {
      write('\n#### function ####');
      write('\nfunctionVar: ' + functionVar); //undefined, but visible
    
      try {
        write('functionLet: ' + functionLet); //undefined, *not* visible
      } catch (exception) {
        write('functionLet: exception');
      }
    
      write('\nset variables');
    
      var functionVar = 'functionVar';
      let functionLet = 'functionLet';
    
      write('\nfunctionVar: ' + functionVar);
      write('functionLet: ' + functionLet);
    }
    
    function blockScoped() {
      write('\n#### block ####');
      write('\nblockVar: ' + blockVar); //undefined, but visible
    
      try {
        write('blockLet: ' + blockLet); //undefined, *not* visible
      } catch (exception) {
        write('blockLet: exception');
      }
    
      for (var blockVar = 'blockVar', blockIndex = 0; blockIndex < 1; blockIndex++) {
        write('\nblockVar: ' + blockVar); // visible here and whole function
      };
    
      for (let blockLet = 'blockLet', letIndex = 0; letIndex < 1; letIndex++) {
        write('blockLet: ' + blockLet); // visible only here
      };
    
      write('\nblockVar: ' + blockVar);
    
      try {
        write('blockLet: ' + blockLet); //undefined, *not* visible
      } catch (exception) {
        write('blockLet: exception');
      }
    }
    
    function write(line) {
      html += (line ? line : '') + '<br />';
    }
    
    functionScoped();
    blockScoped();
    
    document.getElementById('results').innerHTML = html;

    <pre id="results"></pre>

### Global:

They are very similar when used like this outside a function block.

    let me = 'go';  // globally scoped
    var i = 'able'; // globally scoped
    

However, global variables defined with `let` will not be added as properties on the global `window` object like those defined with `var`.

    console.log(window.me); // undefined
    console.log(window.i); // 'able'
    

### Function:

They are identical when used like this in a function block.

    function ingWithinEstablishedParameters() {
        let terOfRecommendation = 'awesome worker!'; //function block scoped
        var sityCheerleading = 'go!'; //function block scoped
    }
    

### Block:

Here is the difference. `let` is only visible in the `for()` loop and `var` is visible to the whole function.

    function allyIlliterate() {
        //tuce is *not* visible out here
    
        for( let tuce = 0; tuce < 5; tuce++ ) {
            //tuce is only visible in here (and in the for() parentheses)
            //and there is a separate tuce variable for each iteration of the loop
        }
    
        //tuce is *not* visible out here
    }
    
    function byE40() {
        //nish *is* visible out here
    
        for( var nish = 0; nish < 5; nish++ ) {
            //nish is visible to the whole function
        }
    
        //nish *is* visible out here
    }
    

### Redeclaration:

Assuming strict mode, `var` will let you re-declare the same variable in the same scope. On the other hand, `let` will not:

    'use strict';
    let me = 'foo';
    let me = 'bar'; // SyntaxError: Identifier 'me' has already been declared
    

    'use strict';
    var me = 'foo';
    var me = 'bar'; // No problem, `me` is replaced.
