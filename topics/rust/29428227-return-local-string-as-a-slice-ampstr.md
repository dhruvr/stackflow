
# Return local String as a slice (&amp;str)

## Question
      
There are several questions that seem to be about the same problem I'm having. For example see [here](https://stackoverflow.com/questions/24542064/why-does-the-variable-not-live-long-enough) and [here](https://stackoverflow.com/questions/26435791/as-slice-produces-not-live-enough-error). Basically I'm trying to build a `String` in a local function, but then return it as a `&str`. Slicing isn't working because the lifetime is too short. I can't use `str` directly in the function because I need to build it dynamically. However, I'd also prefer not to return a `String` since the nature of the object this is going into is static once it's built. Is there a way to have my cake and eat it too?

Here's a minimal non-compiling reproduction:

    fn return_str<'a>() -> &'a str {
        let mut string = "".to_string();
    
        for i in 0..10 {
            string.push_str("ACTG");
        }
    
        &string[..]
    }
## Answer
      
No, you cannot do it. There are at least two explanations why it is so.

First, remember that references are borrowed, i.e. they point to some data but do not own it, it is owned by someone else. In this particular case the string, a slice to which you want to return, is owned by the function because it is stored in a local variable.

When the function exits, all its local variables are destroyed; this involves calling destructors, and the destructor of `String` frees the memory used by the string. However, you want to return a borrowed reference pointing to the data allocated for that string. It means that the returned reference immediately becomes dangling - it points to invalid memory!

Rust was created, among everything else, to prevent such problems. Therefore, in Rust it is impossible to return a reference pointing into local variables of the function, which is possible in languages like C.

There is also another explanation, slightly more formal. Let's look at your function signature:

    fn return_str<'a>() -> &'a str
    

Remember that lifetime and generic parameters are, well, _parameters_: they are set by the caller of the function. For example, some other function may call it like this:

    let s: &'static str = return_str();
    

This requires `'a` to be `'static`, but it is of course impossible - your function does not return a reference to a static memory, it returns a reference with a strictly lesser lifetime. Thus such function definition is unsound and is prohibited by the compiler.

Anyway, in such situations you need to return a value of an owned type, in this particular case it will be an owned `String`:

    fn return_str() -> String {
        let mut string = String::new();
    
        for _ in 0..10 {
            string.push_str("ACTG");
        }
    
        string
    }
    