
# Why can&apos;t I store a value and a reference to that value in the same struct?

## Question
      
I have a value and I want to store that value and a reference to something inside that value in my own type:

    struct Thing {
        count: u32,
    }
    
    struct Combined<'a>(Thing, &'a u32);
    
    fn make_combined<'a>() -> Combined<'a> {
        let thing = Thing { count: 42 };
    
        Combined(thing, &thing.count)
    }
    

Sometimes, I have a value and I want to store that value and a reference to that value in the same structure:

    struct Combined<'a>(Thing, &'a Thing);
    
    fn make_combined<'a>() -> Combined<'a> {
        let thing = Thing::new();
    
        Combined(thing, &thing)
    }
    

Sometimes, I'm not even taking a reference of the value and I get the same error:

    struct Combined<'a>(Parent, Child<'a>);
    
    fn make_combined<'a>() -> Combined<'a> {
        let parent = Parent::new();
        let child = parent.child();
    
        Combined(parent, child)
    }
    

In each of these cases, I get an error that one of the values "does not live long enough". What does this error mean?
## Answer
      
Let's look at [a simple implementation of this](https://play.rust-lang.org/?gist=7b94f2fb1b943d24b79b87fccdd6c995&version=stable&backtrace=0):

    struct Parent {
        count: u32,
    }
    
    struct Child<'a> {
        parent: &'a Parent,
    }
    
    struct Combined<'a> {
        parent: Parent,
        child: Child<'a>,
    }
    
    impl<'a> Combined<'a> {
        fn new() -> Self {
            let p = Parent { count: 42 };
            let c = Child { parent: &p };
    
            Combined { parent: p, child: c }
        }
    }
    
    fn main() {}
    

This will fail with the slightly cleaned up error:

    error: `p` does not live long enough
      --> src/main.rs:17:34
       |
    17 |         let c = Child { parent: &p };
       |                                  ^
       |
    note: reference must be valid for the lifetime 'a as defined
          on the block at 15:21...
      --> src/main.rs:15:22
       |
    15 |     fn new() -> Self {
       |                      ^
    note: ...but borrowed value is only valid for the block suffix
          following statement 0 at 16:37
      --> src/main.rs:16:38
       |
    16 |         let p = Parent { count: 42 };
       |                                      ^
    

To completely understand this error, you have to think about how the values are represented in memory and what happens when you _move_ those values. Let's annotate `Combined::new` with some hypothetical memory addresses that show where values are located:

    let p = Parent { count: 42 };
    // `p` lives at address 0x1000 and takes up 4 bytes
    // The value of `p` is 42 
    let c = Child { parent: &p };
    // `c` lives at address 0x1010 and takes up 4 bytes
    // The value of `c` is 0x1000
    
    Combined { parent: p, child: c }
    // The return value lives at address 0x2000 and takes up 8 bytes
    // `p` is moved to 0x2000
    // `c` is ... ?
    

What should happen to `c`? If the value was just moved like `p` was, then it would refer to memory that no longer is guaranteed to have a valid value in it. Any other piece of code is allowed to store values at memory address 0x1000. Accessing that memory assuming it was an integer could lead to crashes and/or security bugs, and is one of the main categories of errors that Rust prevents.

This is exactly the problem that _lifetimes_ prevent. A lifetime is a bit of metadata that allows you and the compiler to know how long a value will be valid at its **current memory location**. That's an important distinction, as it's a common mistake Rust newcomers make. Rust lifetimes are _not_ the time period between when an object is created and when it is destroyed!

As an analogy, think of it this way: During a person's life, they will reside in many different locations, each with a distinct address. A Rust lifetime is concerned with the address you _currently reside at_, not about whenever you will die in the future (although dying also changes your address). Every time you move it's relevant because your address is no longer valid.

It's also important to note that lifetimes _do not_ change your code; your code controls the lifetimes, your lifetimes don't control the code. The pithy saying is "lifetimes are descriptive, not prescriptive".

Let's annotate `Combined::new` with some line numbers which we will use to highlight lifetimes:

    {                                    // 0
        let p = Parent { count: 42 };    // 1
        let c = Child { parent: &p };    // 2
                                         // 3
        Combined { parent: p, child: c } // 4
    }                                    // 5
    

The _concrete lifetime_ of `p` is from 1 to 4, inclusive (which I'll represent as `[1,4]`). The concrete lifetime of `c` is `[2,4]`, and the concrete lifetime of the return value is `[4,5]`. It's possible to have concrete lifetimes that start at zero - that would represent the lifetime of a parameter to a function or something that existed outside of the block.

Note that the lifetime of `c` itself is `[2,4]`, but that it **refers to** a value with a lifetime of `[1,4]`. This is fine as long as the referring value becomes invalid before the referred-to value does. The problem occurs when we try to return `c` from the block. This would "over-extend" the lifetime beyond its natural length.

This new knowledge should explain the first two examples. The third one requires looking at the implementation of `Parent::child`. Chances are, it will look something like this:

    impl Parent {
        fn child(&self) -> Child { ... }
    }
    

This uses _lifetime elision_ to avoid writing explicit _generic lifetime parameters_. It is equivalent to:

    impl Parent {
        fn child<'a>(&'a self) -> Child<'a> { ... }
    }
    

In both cases, the method says that a `Child` structure will be returned that has been parameterized with the concrete lifetime of `self`. Said another way, the `Child` instance contains a reference to the `Parent` that created it, and thus cannot live longer than that `Parent` instance.

This also lets us recognize that something is really wrong with our creation function:

    fn make_combined<'a>() -> Combined<'a> { ... }
    

Although you are more likely to see this written in a different form:

    impl<'a> Combined<'a> {
        fn new() -> Combined<'a> { ... }
    }
    

In both cases, there is no lifetime parameter being provided via an argument. This means that the lifetime that `Combined` will be parameterized with isn't constrained by anything - it can be whatever the caller wants it to be. This is nonsensical, because the caller could specify the `'static` lifetime and there's no way to meet that condition.

### How do I fix it?

The easiest and most recommended solution is to not attempt to put these items in the same structure together. By doing this, your structure nesting will mimic the lifetimes of your code. Place types that own data into a structure together and then provide methods that allow you to get references or objects containing references as needed.

There is a special case where the lifetime tracking is overzealous: when you have something placed on the heap. This occurs when you use a `Box<T>`, for example. In this case, the structure that is moved contains a pointer into the heap. The pointed-at value will remain stable, but the address of the pointer itself will move. In practice, this doesn't matter, as you always follow the pointer.

The [rental crate](https://crates.io/crates/rental) or the [owning_ref crate](https://crates.io/crates/owning_ref) are ways of representing this case, but they require that the base address _never move_. This rules out mutating vectors, which may cause a reallocation and a move of the heap-allocated values.

### More information

> After moving `p` into the struct, why is the compiler not able to get a new reference to `p` and assign it to `c` in the struct?

While it is theoretically possible to do this, doing so would introduce a large amount of complexity and overhead. Every time that the object is moved, the compiler would need to insert code to "fix up" the reference. This would mean that copying a struct is no longer a very cheap operation that just moves some bits around. It could even mean that code like this is expensive, depending on how good a hypothetical optimizer would be:

    let a = Object::new();
    let b = a;
    let c = b;
    

Instead of forcing this to happen for _every_ move, the programmer gets to _choose_ when this will happen by creating methods that will take the appropriate references only when you call them.

* * *

There's one specific case where you _can_ create a type with a reference to itself. You need to use something like `Option` to make it in two steps though:

    #[derive(Debug)]
    struct WhatAboutThis<'a> {
        name: String,
        nickname: Option<&'a str>,
    }
    
    fn main() {
        let mut tricky = WhatAboutThis {
            name: "Annabelle".to_string(),
            nickname: None,
        };
        tricky.nickname = Some(&tricky.name[..4]);
    
        println!("{:?}", tricky);
    }
    

This does work, in some sense, but the created value is highly restricted - it can _never_ be moved. Notably, this means it cannot be returned from a function or passed by-value to anything. A constructor function shows the same problem with the lifetimes as above:

    fn creator<'a>() -> WhatAboutThis<'a> {
        // ...
    }
    