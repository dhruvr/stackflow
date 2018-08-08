
---
created: 2018-08-09_16-30-16
issue_id: 1
title: How to lookup from and insert into a HashMap efficiently?
---


# How to lookup from and insert into a HashMap efficiently?

## Question
      
I'd like to do the following:

*   Lookup a `Vec` for a certain key, and store it for later use.
*   If it doesn't exist, create an empty `Vec` for the key, but still keep it in the variable.

How to do this efficiently? Naturally I thought I could use `match`:

    use std::collections::HashMap;
    
    // This code doesn't compile.
    let mut map = HashMap::new();
    let key = "foo";
    let values: &Vec<isize> = match map.get(key) {
        Some(v) => v,
        None => {
            let default: Vec<isize> = Vec::new();
            map.insert(key, default);
            &default
        }
    };
    

When I tried it, it gave me errors like:

    error[E0502]: cannot borrow `map` as mutable because it is also borrowed as immutable
      --> src/main.rs:11:13
       |
    7  |     let values: &Vec<isize> = match map.get(key) {
       |                                     --- immutable borrow occurs here
    ...
    11 |             map.insert(key, default);
       |             ^^^ mutable borrow occurs here
    ...
    15 | }
       | - immutable borrow ends here
    

I ended up with doing something like this, but I don't like the fact that it performs the lookup twice (`map.contains_key` and `map.get`):

    // This code does compile.
    let mut map = HashMap::new();
    let key = "foo";
    if !map.contains_key(key) {
        let default: Vec<isize> = Vec::new();
        map.insert(key, default);
    }
    let values: &Vec<isize> = match map.get(key) {
        Some(v) => v,
        None => {
            panic!("impossiburu!");
        }
    };
    

Is there a safe way to do this with just one `match`?
## Answer
      
The [`entry` API](http://doc.rust-lang.org/nightly/std/collections/struct.HashMap.html#method.entry) is designed for this. In manual form, it might look like

    use std::collections::hash_map::Entry;
    
    let values: &Vec<isize> = match map.entry(key) {
        Entry::Occupied(o) => o.into_mut(),
        Entry::Vacant(v) => v.insert(default)
    };
    

Or one can use the briefer form:

    map.entry(key).or_insert_with(|| default)
    

If `default` is OK/cheap to compute even when it isn't inserted, it can also just be:

    map.entry(key).or_insert(default)
    
