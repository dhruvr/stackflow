
# What is the scope of variables in JavaScript?

## Question
        
What is the scope of variables in javascript? Do they have the same scope inside as opposed to outside a function? Or does it even matter? Also, where are the variables stored if they are defined globally?

## Answer
        
I think about the best I can do is give you a bunch of examples to study. Javascript programmers are practically ranked by how well they understand scope. It can at times be quite counter-intuitive.

1.  **A globally-scoped variable**
    
        // global scope
        var a = 1;
        
        function one() {
          alert(a); // alerts '1'
        }
        
    
2.  **Local scope**
    
        // global scope
        var a = 1;
        
        function two(a) {
          // local scope
          alert(a); // alerts the given argument, not the global value of '1'
        }
        
        // local scope again
        function three() {
          var a = 3;
          alert(a); // alerts '3'
        }
        
    
3.  **Intermediate**: _No such thing as block scope in JavaScript_ (ES5; ES6 introduces [`let`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/let))
    
    a.
    
        var a = 1;
        
        function four() {
          if (true) {
            var a = 4;
          }
        
          alert(a); // alerts '4', not the global value of '1'
        }
        
    
    b.
    
        var a = 1;
        
        function one() {
          if (true) {
            let a = 4;
          }
        
          alert(a); // alerts '1' because the 'let' keyword uses block scoping
        }
        
    
4.  **Intermediate**: _Object properties_
    
        var a = 1;
        
        function Five() {
          this.a = 5;
        }
        
        alert(new Five().a); // alerts '5'
        
    
5.  **Advanced**: _Closure_
    
        var a = 1;
        
        var six = (function() {
          var a = 6;
        
          return function() {
            // JavaScript "closure" means I have access to 'a' in here,
            // because it is defined in the function in which I was defined.
            alert(a); // alerts '6'
          };
        })();
        
    
6.  **Advanced**: _Prototype-based scope resolution_
    
        var a = 1;
        
        function seven() {
          this.a = 7;
        }
        
        // [object].prototype.property loses to
        // [object].property in the lookup chain. For example...
        
        // Won't get reached, because 'a' is set in the constructor above.
        seven.prototype.a = -1;
        
        // Will get reached, even though 'b' is NOT set in the constructor.
        seven.prototype.b = 8;
        
        alert(new seven().a); // alerts '7'
        alert(new seven().b); // alerts '8'
        
    
    * * *
    
7.  **Global+Local**: _An extra complex Case_
    
        var x = 5;
        
        (function () {
            console.log(x);
            var x = 10;
            console.log(x); 
        })();
        
    
    This will print out `undefined` and `10` rather than `5` and `10` since JavaScript always moves variable declarations (not initializations) to the top of the scope, making the code equivalent to:
    
        var x = 5;
        
        (function () {
            var x;
            console.log(x);
            x = 10;
            console.log(x); 
        })();
        
    
8.  **Catch clause-scoped variable**
    
        var e = 5;
        console.log(e);
        try {
            throw 6;
        } catch (e) {
            console.log(e);
        }
        console.log(e);
        
    
    This will print out `5`, `6`, `5`. Inside the catch clause `e` shadows global and local variables. But this special scope is only for the caught variable. If you write `var f;` inside the catch clause, then it's exactly the same as if you had defined it before or after the try-catch block.
