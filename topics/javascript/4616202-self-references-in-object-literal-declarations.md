
# Self-references in object literal declarations

## Question
        
Is there any way to get something like the following to work in JavaScript?

    var foo = {
        a: 5,
        b: 6,
        c: this.a + this.b  // Doesn't work
    };
    

In the current form, this code obviously throws a reference error since `this` doesn't refer to `foo`. But _is_ there any way to have values in an object literal's properties depend on other properties declared earlier?

## Answer
        
Well, the only thing that I can tell you about are getters:

    var foo = {
      a: 5,
      b: 6,
      get c () {
        return this.a + this.b;
      }
    };
    
    foo.c; // 11
    

This is a syntactic extension introduced by the ECMAScript 5th Edition Specification, the syntax is supported by most modern browsers (including IE9).
