
# Cannot obtain a mutable reference when iterating a recursive structure: cannot borrow as mutable more than once at a time

## Question
      
I'm trying to navigate a recursive data structure iteratively in order to insert elements at a certain position. To my limited understanding, this means taking a mutable reference to the root of the structure and successively replacing it by a reference to its follower:

    type Link = Option<Box<Node>>;
    
    struct Node {
        next: Link
    }
    
    struct Recursive {
        root: Link
    }
    
    impl Recursive {
        fn back(&mut self) -> &mut Link {
            let mut anchor = &mut self.root;
            while let Some(ref mut node) = *anchor {
                anchor = &mut node.next;
            }
            anchor
        }
    }
    

[(Rust playground link)](https://play.rust-lang.org/?gist=119706859a0013c7f11dd8ad9c284386&version=stable&backtrace=0)

However, this fails:

    error[E0499]: cannot borrow `anchor.0` as mutable more than once at a time
      --> src/main.rs:14:24
       |
    14 |         while let Some(ref mut node) = *anchor {
       |                        ^^^^^^^^^^^^
       |                        |
       |                        second mutable borrow occurs here
       |                        first mutable borrow occurs here
    ...
    18 |     }
       |     - first borrow ends here
    
    error[E0506]: cannot assign to `anchor` because it is borrowed
      --> src/main.rs:15:13
       |
    14 |         while let Some(ref mut node) = *anchor {
       |                        ------------ borrow of `anchor` occurs here
    15 |             anchor = &mut node.next;
       |             ^^^^^^^^^^^^^^^^^^^^^^^ assignment to borrowed `anchor` occurs here
    
    error[E0499]: cannot borrow `*anchor` as mutable more than once at a time
      --> src/main.rs:17:9
       |
    14 |         while let Some(ref mut node) = *anchor {
       |                        ------------ first mutable borrow occurs here
    ...
    17 |         anchor
       |         ^^^^^^ second mutable borrow occurs here
    18 |     }
       |     - first borrow ends here
    

This makes sense as both `anchor` and `node` refer to the same structure, but I actually don't care about `anchor` any more after destructuring it.

How could `back()` be implemented correctly using safe Rust?
## Answer
      
It is possible... but I wish I had a more elegant solution.

The trick is NOT to borrow from `anchor`, and therefore to juggle between two accumulators:

*   one holding the reference to the current node
*   the other being assigned the reference to the next node

This leads me to:

    impl Recursive {
        fn back(&mut self) -> &mut Link {
            let mut anchor = &mut self.root;
    
            loop {
                let tmp = anchor;
                if let Some(ref mut node) = *tmp {
                    anchor = &mut node.next;
                } else {
                    anchor = tmp;
                    break;
                }
            }
    
            anchor
        }
    }
    

Not exactly pretty, but this is something the borrow checker can get behind so ¯\\_(ツ)_/¯.

@ker has improved on this by creating an unnamed temporary:

    impl Recursive {
        fn back(&mut self) -> &mut Link {
            let mut anchor = &mut self.root;
    
            loop {
                match {anchor} {
                    &mut Some(ref mut node) => anchor = &mut node.next,
                    other => return other,
                }
            }
        }
    }
    

The trick here is that using `{anchor}` _moves_ the content of `anchor` into an unnamed temporary on which the match executes. Therefore, in the `match` block we are not borrowing from `anchor` but from the temporary, leaving us free to modify `anchor`. See the related blog post [Stuff the Identity Function Does (in Rust)](https://bluss.github.io/rust/fun/2015/10/11/stuff-the-identity-function-does/).
    