
# Prototypical inheritance - writing up [duplicate]

## Question
        
This question already has an answer here:

*   [Javascript object members that are prototyped as arrays become shared by all class instances](/questions/4425318/javascript-object-members-that-are-prototyped-as-arrays-become-shared-by-all-cla) 3 answers

So I have these 2 examples, from javascript.info:

Example 1:

    var animal = {
      eat: function() {
        alert( "I'm full" )
        this.full = true
      }
    }
    
    var rabbit = {
      jump: function() { /* something */ }
    }
    
    rabbit.__proto__ = animal 
    
    rabbit.eat() 
    

Example 2:

    function Hamster() {  }
    Hamster.prototype = {
      food: [],
      found: function(something) {
        this.food.push(something)
      }
    }
    
    // Create two speedy and lazy hamsters, then feed the first one
    speedy = new Hamster()
    lazy = new Hamster()
    
    speedy.found("apple")
    speedy.found("orange")
    
    alert(speedy.food.length) // 2
    alert(lazy.food.length) // 2 (!??)
    

Start from Example 2: when the code reaches `speedy.found`, it finds no `found` property in `speedy`, and so it climbs up to the prototype and changes it there. That's why `food.length` is equal for both hamsters, in other words they have the same stomach.  
  
From this I understand, that when writing up and adding a new property which doesn't exist, the interpreter will go up the prototype chain until it finds the property, and THEN change it.

BUT in Example 1 something else happens:  
we run `rabbit.eat`, which changes `rabbit.full`. `full` property is nowhere to be found, so it should go up the prototype chain to (to object??), and well, I'm not sure what happens here. In this example the property `full` of `rabbit` is created and changed, while in the first example it goes up the prototype chain because it cannot find the property.

I'm confused and cannot see why this happens.

## Answer
        
**Constructor function introduction**

You can use a function as a constructor to create objects, if the constructor function is named Person then the object(s) created with that constructor are instances of Person.

    var Person = function(name){
      this.name = name;
    };
    Person.prototype.walk=function(){
      this.step().step().step();
    };
    var bob = new Person("Bob");
    

Person is the constructor function. When you create an instance using Person you have to use the new keyword:

    var bob = new Person("Bob");console.log(bob.name);//=Bob
    var ben = new Person("Ben");console.log(ben.name);//=Ben
    

The property/member `name` is instance specific, it's different for bob and ben

The member `walk` is part of Person.prototype and is shared for all instances bob and ben are instances of Person so they share the walk member (bob.walk===ben.walk).

    bob.walk();ben.walk();
    

Because walk() could not be found on bob directly JavaScript will look for it in the Person.prototype as this is the constructor of bob. If it can't be found there it'll look on Object.prototype. This is called the prototype chain. The prototype part of inheritance is done by lengthening this chain; for example bob => Employee.prototype => Person.prototype => Object.prototype (more on inheritance later).

Even though bob, ben and all other created Person instances share walk the function will behave differently per instance because in the walk function it uses `this`. The value of `this` will be the invoking object; for now let's say it's the current instance so for `bob.walk()` "this" will be bob. (more on "this" and the invoking object later).

If ben was waiting for a red light and and bob was at a green light; then you'll invoke walk() on both ben and bob obviously something different would happen to ben and bob.

Shadowing members happens when we do something like `ben.walk=22`, even though bob and ben share `walk` the _assignment_ of 22 to ben.walk will not affect bob.walk. This is because that statement will create a member called `walk` on ben directly and assign it a value of 22. There will be 2 different walk members: ben.walk and Person.prototype.walk.

When asking for bob.walk you'll get the Person.prototype.walk function because `walk` could not be found on bob. Asking for ben.walk however will get you the value 22 because the member walk has been created on ben and since JavaScript found walk on ben it will not look in the Person.prototype.

When using Object.create with 2 arguments, Object.defineProperty or Object.defineProperties shadowing works a bit different. More info on that [here](https://github.com/getify/You-Dont-Know-JS/blob/master/this%20%26%20object%20prototypes/ch5.md#setting--shadowing-properties).

**More about prototype**

An object can inherit from another object through the use of prototype. You can set the prototype of any object with any other object using `Object.create`. In the constructor function introduction we have seen that if a member can't be found on the object then JavaScript will look in the prototpe chain for it.

In previous part we have seen that re assignment of members that come from an instance's prototype (ben.walk) will shadow that member (create walk on ben rather than changing Person.prototype.walk).

What if we don't re assign but mutate the member? Mutating is (for example) changing sub properties of an Object or invoking functions that will change the object's value. For example:

    var o = [];
    var a = o;
    a.push(11);//mutate a, this will change o
    a[1]=22;//mutate a, this will change o
    

The following code demonstrates the difference between prototype members and instance members by mutating members.

    var person = {
      name:"default",//immutable so can be used as default
      sayName:function(){
        console.log("Hello, I am "+this.name);
      },
      food:[]//not immutable, should be instance specific
             //  not suitable as prototype member
    };
    var ben = Object.create(person);
    ben.name = "Ben";
    var bob = Object.create(person);
    console.log(bob.name);//=default, setting ben.name shadowed the member
                          //  so bob.name is actually person.name
    ben.food.push("Hamburger");
    console.log(bob.food);//=["Hamburger"], mutating a shared member on the
    // prototype affects all instances as it changes person.food
    console.log(person.food);//=["Hamburger"]
    

The code above shows that ben and bob share members from person. There is only one person, it is set as bob's and ben's prototype (person is used as the first object in the prototype chain to look up requested members that don't exist on the instance). The problem with the above code is that bob and ben should have their own `food` member. This is where the constructor function comes in. It is used to create instance specific members. You could also pass arguments to it to set values of these instance specific members.

The next code shows another way to implement the constructor function, syntax is different but the idea is the same:

1.  Define an object that has members that will be same for many instances (person is a blueprint for bob and ben and can be for jilly, marie, clair ...)
2.  Define instance specific members that should be unique for instances (bob and ben).
3.  Create an instance running the code in step 2.

With constructor functions you'll set the prototype in step 2 in the following code we set the prototype in step 3.

In this code I have removed name from prototype as well as food because you are most likely going to shadow this almost immediately when creating an instance anyway. Name is now an instance specific member with a default value set in the constructor function. Becaus the food member is also moved from prototype to instance specific member it will not affect bob.food when adding food to ben.

    var person = {
      sayName:function(){
        console.log("Hello, I am "+this.name);
      },
      //need to run the constructor function when creating
      //  an instance to make sure the instance has
      //  instance specific members
      constructor:function(name){
        this.name = name || "default";
        this.food = [];
        return this;
      }
    };
    var ben = Object.create(person).constructor("Ben");
    var bob = Object.create(person).constructor("Bob");
    console.log(bob.name);//="Bob"
    ben.food.push("Hamburger");
    console.log(bob.food);//=[]
    

You may come across similar patterns that are more robust to help with object creation and object definition.

**Inheritance**

The following code shows how to inherit. The tasks are basically the same as in code before with a little extra

1.  Define instance specific members of an object (functions Hamster and RussionMini).
2.  Set the prototype part of inheritance (RussionMini.prototype = Object.create(Hamster.prototype))
3.  Define members that can be shared among instances.(Hamster.prototype and RussionMini.prototype)
4.  Create an instance running the code in step 1 and for objects that inherit have them run the Parent code as well (Hamster.apply(this,arguments);)

Using a pattern some would call "classical inheritance". If you are confused by the syntax I'll be happy to explain more or provide different patterns.

    function Hamster(){
     this.food=[];
    }
    function RussionMini(){
      //Hamster.apply(this,arguments) executes every line of code
      //in the Hamster body where the value of "this" is
      //the to be created RussionMini (once for mini and once for betty)
      Hamster.apply(this,arguments);
    }
    //setting RussionMini's prototype
    RussionMini.prototype=Object.create(Hamster.prototype);
    //setting the built in member called constructor to point
    // to the right function (previous line has it point to Hamster)
    RussionMini.prototype.constructor=RussionMini;
    mini=new RussionMini();
    //this.food (instance specic to mini)
    //  comes from running the Hamster code
    //  with Hamster.apply(this,arguments);
    mini.food.push("mini's food");
    //adding behavior specific to Hamster that will still be
    //  inherited by RussionMini because RussionMini.prototype's prototype
    //  is Hamster.prototype
    Hamster.prototype.runWheel=function(){console.log("I'm running")};
    mini.runWheel();//=I'm running
    

**Object.create to set prototype part of inheritance**

Here is the documentation about [Object.create](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Object/create), it basically returns the second argument (not supported in the polyfil) with the first argument as the returned object's prototype.

If no second argument was given it'll return an empty object with first argument to be used as the returned object's prototype (the first object to be used in the returned object's prototype chain).

Some would set the prototype of RussionMini to an instance of Hamster (RussionMini.prototype = new Hamster()). This is not desirable because even though it accomplishes the same (RussionMini.prototype's prototype is Hamster.prototype) it also sets Hamster instance members as members of RussionMini.prototype. So RussionMini.prototype.food will exist but is a shared member (remember bob and ben in "More about prototype"?). The food member will be shadowed when creating a RussionMini because Hamster code is run with `Hamster.apply(this,arguments);` that in turn runs `this.food = []` but any Hamster members will still be members of RussionMini.prototype.

Another reason could be that to create a Hamster a lot of complicated calculations need be done on passed arguments that may be not available yet, again you could pass in dummy arguments but it could unnecessarily complicate your code.

**Extending and overriding Parent functions**

Sometimes `children` need to extend `parent` functions.

You want the 'child' (=RussionMini) to do something extra. When RussionMini can call the Hamster code to do something and then do something extra you don't need to copy and paste Hamster code to RussionMini.

In the following example we assume that a Hamster can run 3km an hour but a Russion mini can only run half as fast. We can hard code 3/2 in RussionMini but if this value were to change we have multiple places in code where it needs changing. Here is how we use Hamster.prototype to get the parent (Hamster) speed.

    var Hamster = function(name){
     if(name===undefined){
       throw new Error("Name cannot be undefined");
     }
     this.name=name;
    }
    Hamster.prototype.getSpeed=function(){
      return 3;
    }
    Hamster.prototype.run=function(){
      //Russionmini does not need to implement this function as
      //it will do exactly the same as it does for Hamster
      //But Russionmini does need to implement getSpeed as it
      //won't return the same as Hamster (see later in the code) 
      return "I am running at " + 
        this.getSpeed() + "km an hour.";
    }
    
    var RussionMini=function(name){
      Hamster.apply(this,arguments);
    }
    //call this before setting RussionMini prototypes
    RussionMini.prototype = Object.create(Hamster.prototype);
    RussionMini.prototype.constructor=RussionMini;
    
    RussionMini.prototype.getSpeed=function(){
      return Hamster.prototype
        .getSpeed.call(this)/2;
    }    
    
    var betty=new RussionMini("Betty");
    console.log(betty.run());//=I am running at 1.5km an hour.
    

The disadvantage is that you hard code Hamster.prototype. There may be patterns that will give you the advantage of `super` as in Java.

Most of the patterns I've seen will either break when inheritance level is more than 2 levels (Child => Parent => GrandParent) or use more resources by implementing super through [closures](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Closures).

To override a Parent (=Hamster) method you do the same but don't do Hamster.prototype.parentMethod.call(this,....

**this.constructor**

The constructor property is included in the prototype by JavaScript, you can change it but it should point to the constructor function. So `Hamster.prototype.constructor` should point to Hamster.

If after setting prototype part of inheritance you should have it point to the right function again.

    var Hamster = function(){};
    var RussionMinni=function(){
       // re use Parent constructor (I know there is none there)
       Hamster.apply(this,arguments);
    };
    RussionMinni.prototype=Object.create(Hamster.prototype);
    console.log(RussionMinni.prototype.constructor===Hamster);//=true
    RussionMinni.prototype.haveBaby=function(){
      return new this.constructor();
    };
    var betty=new RussionMinni();
    var littleBetty=betty.haveBaby();
    console.log(littleBetty instanceof RussionMinni);//false
    console.log(littleBetty instanceof Hamster);//true
    //fix the constructor
    RussionMinni.prototype.constructor=RussionMinni;
    //now make a baby again
    var littleBetty=betty.haveBaby();
    console.log(littleBetty instanceof RussionMinni);//true
    console.log(littleBetty instanceof Hamster);//true
    

**"Multiple inheritance" with mix ins**

Some things are better not to be inherited, if a Cat can move and then a Cat should not inherit from Movable. A Cat is not a Movable but rather a Cat can move. In a class based language Cat would have to implement Movable. In JavaScript we can define Movable and define implementation here, Cat can either override, extend it or us it's default implementation.

For Movable we have instance specific members (like `location`). And we have members that are not instance specific (like the function move()). Instance specific members will be set by calling mxIns (added by mixin helper function) when creating an instance. Prototype members will be copied one by one on Cat.prototype from Movable.prototype using the mixin helper function.

    var Mixin = function Mixin(args){
      if(this.mixIns){
        i=-1;len=this.mixIns.length;
        while(++i<len){
            this.mixIns[i].call(this,args);
          }
      }  
    };
    Mixin.mix = function(constructor, mix){
      var thing
      ,cProto=constructor.prototype
      ,mProto=mix.prototype;
      //no extending, if multiple prototypes
      // have members with the same name then use
      // the last
      for(thing in mProto){
        if(Object.hasOwnProperty.call(mProto, thing)){
          cProto[thing]=mProto[thing];
        }
      }
      //instance intialisers
      cProto.mixIns = cProto.mixIns || [];
      cProto.mixIns.push(mix);
    };
    var Movable = function(args){
      args=args || {};
      //demo how to set defaults with truthy
      // not checking validaty
      this.location=args.location;
      this.isStuck = (args.isStuck===true);//defaults to false
      this.canMove = (args.canMove!==false);//defaults to true
      //speed defaults to 4
      this.speed = (args.speed===0)?0:(args.speed || 4);
    };
    Movable.prototype.move=function(){
      console.log('I am moving, default implementation.');
    };
    var Animal = function(args){
      args = args || {};
      this.name = args.name || "thing";
    };
    var Cat = function(args){
      var i,len;
      Animal.call(args);
      //if an object can have others mixed in
      //  then this is needed to initialise 
      //  instance members
      Mixin.call(this,args);
    };
    Cat.prototype = Object.create(Animal.prototype);
    Cat.prototype.constructor = Cat;
    Mixin.mix(Cat,Movable);
    var poochie = new Cat({
      name:"poochie",
      location: {x:0,y:22}
    });
    poochie.move();
    

The above is a simple implementation that replaces same named functions with whatever mix in is mixed in last.

**The this variable**

In all the example code you'll see `this` referring to the current instance.

The this variable actually refers to the invoking object, it refers to the object that came before the function.

To clarify see the following code:

    theInvokingObject.thefunction();
    

The instances where this would refer to the wrong object are usually when attaching event listeners, callbacks or timeouts and intervals. In the next 2 lines of code we `pass` the function, we don't invoke it. Passing the function is: `someObject.aFunction` and invoking it is: `someObject.aFunction()`. The `this` value does not refer to the object the function was declared on but on the object that `invokes` it.

    setTimeout(someObject.aFuncton,100);//this in aFunction is window
    somebutton.onclick = someObject.aFunction;//this in aFunction is somebutton
    

To make `this` in the above cases refer to someObject you can pass a [closure](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Closures) instead of the function directly:

    setTimeout(function(){someObject.aFuncton();},100);
    somebutton.onclick = function(){someObject.aFunction();};
    

I like to define functions that return a function for [closures](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Closures) on the prototype to have fine control over the variables that are included in the [closure](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Closures) scope.

    var Hamster = function(name){
      var largeVariable = new Array(100000).join("Hello World");
      // if I do 
      // setInterval(function(){this.checkSleep();},100);
      // then largeVariable will be in the closure scope as well
      this.name=name
      setInterval(this.closures.checkSleep(this),1000);
    };
    Hamster.prototype.closures={
      checkSleep:function(hamsterInstance){
        return function(){
          console.log(typeof largeVariable);//undefined
          console.log(hamsterInstance);//instance of Hamster named Betty
          hamsterInstance.checkSleep();
        };
      }
    };
    Hamster.prototype.checkSleep=function(){
      //do stuff assuming this is the Hamster instance
    };
    
    var betty = new Hamster("Betty");
    

**Passing (constructor) arguments**

When Child calls a Parent (`Hamster.apply(this,arguments);`) we assume that Hamster uses the same arguments as RussionMini in the same order. For functions that call other functions I usually use another way to pass arguments.

I usually pass one object to a function and have that function mutate whatever it needs (set defaults), then that function will pass it to another function that will do the same and so on and so on. Here is an example:

    //helper funciton to throw error
    function thowError(message){
      throw new Error(message)
    };
    var Hamster = function(args){
      //make sure args is something so you get the errors
      //  that make sense to you instead of "args is undefined"
      args = args || {};
      //default value for type:
      this.type = args.type || "default type";
      //name is not optional, very simple truthy check f
      this.name = args.name || thowError("args.name is not optional");
    };
    var RussionMini = function(args){
      //make sure args is something so you get the errors
      //  that make sense to you instead of "args is undefined"
      args = args || {};
      args.type = "Russion Mini";
      Hamster.call(this,args);
    };
    var ben = new RussionMini({name:"Ben"});
    console.log(ben);// Object { type="Russion Mini", name="Ben"}
    var betty = new RussionMini();//Error: args.name is not optional
    

This way of passing arguments in a function chain is useful in many cases. When you're working on code that would calculate a total of something and later you'd like to re factor the total of that something to be in a certain currency you may have to change a lot of functions to pass the value for currency. You could up scope a currency value (even to global like `window.currency='USD'`) but that's a bad way to solve it.

With passing an object you could add currency to `args` whenever it's available in the function chain and mutate/use it whenever you need it without changing the other functions (explicitly have to pass it in the function calls).

**Private variables**

JavaScript doesn't have a private modifier.

I agree with the following: [http://blog.millermedeiros.com/a-case-against-private-variables-and-functions-in-javascript/](http://blog.millermedeiros.com/a-case-against-private-variables-and-functions-in-javascript/) and personally have not used them.

You can indicate to other programmers a member is meant to be private by naming it `_aPrivate` or putting all the private variables in an object variable called `_`.

You can implement private members through [closures](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Closures) but instance specific private members can only be accessed by functions that are not on the prototype.

Not implementing privates as closures would leak implementation and enable you or users extending your code to use members that are not part of your public API. This can be both good and bad.

It's good because it enables you and others to mock certain members for testing easily. It gives others a chance to easily improve (patch) your code but this is also bad because there is no guarantee that a next version of your code has the same implementation and or private members.

By using closures you do not give others a choice and by using the naming convention with documentation you do. This is not specific to JavaScript, in other languages you can decide not to use private members as you trust others to know what they are doing and give them the choice to do as they want (with risks involved).

If you still insist on privates then the [following](https://stackoverflow.com/a/21800194/1641941) pattern may help. It doesn't implement private though but implements protected.
