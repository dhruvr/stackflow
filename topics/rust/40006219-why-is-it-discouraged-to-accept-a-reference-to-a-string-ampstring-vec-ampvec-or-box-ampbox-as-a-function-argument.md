
# Why is it discouraged to accept a reference to a String (&amp;String), Vec (&amp;Vec) or Box (&amp;Box) as a function argument?

## Question
      
I wrote some Rust code that takes a `&String` as an argument:

    fn awesome_greeting(name: &String) {
        println!("Wow, you are awesome, {}!", name);
    }
    

I've also written code that takes in a reference to a `Vec` or `Box`:

    fn total_price(prices: &Vec<i32>) -> i32 {
        prices.iter().sum()
    }
    
    fn is_even(value: &Box<i32>) -> bool {
        **value % 2 == 0
    }
    

However, I received some feedback that doing it like this isn't a good idea. Why not?
## Answer
      
**TL;DR: One can instead use `&str`, `&[T]` or `&T` with no loss of genericity.**

* * *

1.  One of the main reasons to use a `String` or a `Vec` is because they allow increasing or decreasing the capacity. However, when you accept an immutable reference, you cannot use any of those interesting methods on the `Vec` or `String`.
    
2.  Accepting a `&String`, `&Vec` or `&Box` also **requires** an allocation before you can call the method. Unnecessary allocation is a performance loss. This is usually exposed right away when you try to call these methods in a test or a `main` method:
    
        awesome_greeting(&String::from("Anna"));
        
    
        total_price(&vec![42, 13, 1337])
        
    
        is_even(&Box::new(42))
        
    
3.  Another performance consideration is that `&String`, `&Vec` and `&Box` introduce an unnecessary layer of indirection as you have dereference the `&String` to get a `String` and then a second dereference to end up at `&str`.
    

Instead, you should accept a _string slice_ (`&str`), a _slice_ (`&[T]`), or just a reference `&T`. A `&String`, `&Vec<T>` or `&Box<T>` will be automatically coerced to a `&str`, `&[T]` or `&T`, respectively.

    fn awesome_greeting(name: &str) {
        println!("Wow, you are awesome, {}!", name);
    }
    

    fn total_price(prices: &[i32]) -> i32 {
        prices.iter().sum()
    }
    

    fn is_even(value: &i32) -> bool {
        *value % 2 == 0
    }
    

Now you can call these methods with a broader set of types. For example, `awesome_greeting` can be called with a string literal (`"Anna"`) _or_ an allocated `String`. `total_price` can be called with a reference to an array (`&[1, 2, 3]`) _or_ an allocated `Vec`.

* * *

If you'd like to add or remove items from the `String` or `Vec<T>`, you can take a _mutable reference_ (`&mut String` or `&mut Vec<T>`):

    fn add_greeting_target(greeting: &mut String) {
        greeting.push_str("world!");
    }
    

    fn add_candy_prices(prices: &mut Vec<i32>) {
        prices.push(5);
        prices.push(25);
    }
    

Specifically for slices, you can also accept a `&mut [T]`. This allows you to mutate a specific value inside the slice, but you cannot change the number of items inside the slice:

    fn reset_first_price(prices: &mut [i32]) {
        prices[0] = 0;
    }
    