
# Extending an existing type in OCaml

## Question
        
I've been doing some OCaml programming lately to learn the language and to get more acquainted with functional programming. Recently, I've started to think that I'd like to be able to extend an existing type (either built in-or one of my own), for example:

    type bexp =
      And of bexp * bexp
    | Or of bexp * bexp
    | Xor of bexp * bexp
    | Not of bexp;;
    

Now let's say I want to add a Nop variant to this type, but only for use in a new type - kind of like inheritance. Hey, these are supposed to be Algebraic data types, right? So why not something like:

    type nbexp = bexp | Nop nbexp ;;
    

...but this isn't valid OCaml, it gives a syntax error. Basically, what I'm trying to do is say that I want nbexp to include everything bexp includes and also add a Nop to that. I suppose this isn't possible because, if for example you used the And constructor there would be no way to determine if it was a bexp type or a nbexp type. ( I think the constructor Nop taking a nbexp may also be problematic.)

So is there any way to do something like this in OCaml? And, is this the sort of thing that's doable in Haskell (with typeclasses, perhaps)?

## Answer
        
An interesting solution is to use polymorphic variant:

    type bexp =
    [ `And of bexp * bexp
    | `Or of bexp * bexp
    | `Xor of bexp * bexp
    | `Not of bexp ];;
    
    type nbexp = [ bexp | `Nop of nbexp ];;
    

Note that polymorphic variants are trickier than normal ones, but allow extension of type.

An interesting example of expression evaluation, with extension, using polymorphic variant can be found in a test directories of the ocaml source, see the [svn](http://caml.inria.fr/cgi-bin/viewvc.cgi/ocaml/trunk/testlabl/mixin.ml?diff_format=c&view=markup&pathrev=10238)
