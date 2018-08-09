
# How would I translate a Haskell type class into F#?

## Question
        
I'm trying to translate the Haskell core library's Arrows into F# (I think it's a good exercise to understanding Arrows and F# better, and I might be able to use them in a project I'm working on.) However, a direct translation isn't possible due to the difference in paradigms. Haskell uses type-classes to express this stuff, but I'm not sure what F# constructs best map the functionality of type-classes with the idioms of F#. I have a few thoughts, but figured it best to bring it up here and see what was considered to be the closest in functionality.

**For the tl;dr crowd:** How do I translate type-classes (a Haskell idiom) into F# idiomatic code?

For those accepting of my long explanation:

This code from the Haskell standard lib is an example of what I'm trying to translate:

    class Category cat where
        id :: cat a a
        comp :: cat a b -> cat b c -> cat a c
    class Category a => Arrow a where
        arr :: (b -> c) -> a b c
        first :: a b c -> a (b,d) (c,d)
    
    instance Category (->) where
        id f = f
    instance Arrow (->) where
        arr f = f
        first f = f *** id
    

**Attempt 1: Modules, Simple Types, Let Bindings**

My first shot at this was to simply map things over directly using Modules for organization, like:

    type Arrow<'a,'b> = Arrow of ('a -> 'b)
    
    let arr f = Arrow f
    let first f = //some code that does the first op
    

That works, but it loses out on polymorphism, since I don't implement Categories and can't easily implement more specialized Arrows.

**Attempt 1a: Refining using Signatures and types**

One way to correct some issues with Attempt 1 is to use a .fsi file to define the methods (so the types enforce easier) and to use some simple type tweaks to specialize.

    type ListArrow<'a,'b> = Arrow<['a],['b]>
    //or
    type ListArrow<'a,'b> = LA of Arrow<['a],['b]>
    

But the fsi file can't be reused (to enforce the types of the let bound functions) for other implementations, and the type renaming/encapsulating stuff is tricky.

**Attempt 2: Object models and interfaces**

Rationalizing that F# is built to be OO also, maybe a type hierarchy is the right way to do this.

    type IArrow<'a,'b> =
        abstract member comp : IArrow<'b,'c> -> IArrow<'a,'c>
    type Arrow<'a,'b>(func:'a->'b) = 
        interface IArrow<'a,'b> with
            member this.comp = //fun code involving "Arrow (fun x-> workOn x) :> IArrow"
    

Aside from how much of a pain it can be to get what should be static methods (like comp and other operators) to act like instance methods, there's also the need to explicitly upcast the results. I'm also not sure that this methodology is still capturing the full expressiveness of type-class polymorphism. It also makes it hard to use things that MUST be static methods.

**Attempt 2a: Refining using type extensions**

So one more potential refinement is to declare the interfaces as bare as possible, then use extension methods to add functionality to all implementing types.

    type IArrow<'a,'b> with
        static member (&&&) f = //code to do the fanout operation
    

Ah, but this locks me into using one method for all types of IArrow. If I wanted a slightly different (&&&) for ListArrows, what can I do? I haven't tried this method yet, but I would guess I can shadow the (&&&), or at least provide a more specialized version, but I feel like I can't enforce the use of the correct variant.

**Help me**

So what am I supposed to do here? I feel like OO should be powerful enough to replace type-classes, but I can't seem to figure out how to make that happen in F#. Were any of my attempts close? Are any of them "as good as it gets" and that'll have to be good enough?

## Answer
        
Here's the approach I use to simulate Typeclasses (from [http://code.google.com/p/fsharp-typeclasses/](http://code.google.com/p/fsharp-typeclasses/) ).

In your case, for Arrows could be something like this:

    let inline i2 (a:^a,b:^b     ) =                                                      
        ((^a or ^b      ) : (static member instance: ^a* ^b     -> _) (a,b  ))
    let inline i3 (a:^a,b:^b,c:^c) =                                                          
        ((^a or ^b or ^c) : (static member instance: ^a* ^b* ^c -> _) (a,b,c))
    
    type T = T with
        static member inline instance (a:'a      ) = 
            fun x -> i2(a   , Unchecked.defaultof<'r>) x :'r
        static member inline instance (a:'a, b:'b) = 
            fun x -> i3(a, b, Unchecked.defaultof<'r>) x :'r
    
    
    type Return = Return with
        static member instance (_Monad:Return, _:option<'a>) = fun x -> Some x
        static member instance (_Monad:Return, _:list<'a>  ) = fun x  ->    [x]
        static member instance (_Monad:Return, _: 'r -> 'a ) = fun x _ ->    x
    let inline return' x = T.instance Return x
    
    type Bind = Bind with
        static member instance (_Monad:Bind, x:option<_>, _:option<'b>) = fun f -> 
            Option.bind  f x
        static member instance (_Monad:Bind, x:list<_>  , _:list<'b>  ) = fun f -> 
            List.collect f x
        static member instance (_Monad:Bind, f:'r->'a, _:'r->'b) = fun k r -> k (f r) r
    let inline (>>=) x (f:_->'R) : 'R = T.instance (Bind, x) f
    let inline (>=>) f g x    = f x >>= g
    
    type Kleisli<'a, 'm> = Kleisli of ('a -> 'm)
    let runKleisli (Kleisli f) = f
    
    type Id = Id with
        static member        instance (_Category:Id, _: 'r -> 'r     ) = fun () -> id
        static member inline instance (_Category:Id, _:Kleisli<'a,'b>) = fun () ->
            Kleisli return'
    let inline id'() = T.instance Id ()
    
    type Comp = Comp with
        static member        instance (_Category:Comp,         f, _) = (<<) f
        static member inline instance (_Category:Comp, Kleisli f, _) =
            fun (Kleisli g) -> Kleisli (g >=> f)
    
    let inline (<<<) f g = T.instance (Comp, f) g
    let inline (>>>) g f = T.instance (Comp, f) g
    
    type Arr = Arr with
        static member        instance (_Arrow:Arr, _: _ -> _) = fun (f:_->_) -> f
        static member inline instance (_Arrow:Arr, _:Kleisli<_,_>) = 
            fun f -> Kleisli (return' <<< f)
    let inline arr f = T.instance Arr f
    
    type First = First with
        static member        instance (_Arrow:First, f, _: 'a -> 'b) = 
            fun () (x,y) -> (f x, y)
        static member inline instance (_Arrow:First, Kleisli f, _:Kleisli<_,_>) =
            fun () -> Kleisli (fun (b,d) -> f b >>= fun c -> return' (c,d))
    let inline first f = T.instance (First, f) ()
    
    let inline second f = let swap (x,y) = (y,x) in arr swap >>> first f >>> arr swap
    let inline ( *** ) f g = first f >>> second g
    let inline ( &&& ) f g = arr (fun b -> (b,b)) >>> f *** g
    

Usage:

    > let f = Kleisli (fun y -> [y;y*2;y*3]) <<< Kleisli ( fun x -> [ x + 3 ; x * 2 ] ) ;;
    val f : Kleisli<int,int list> = Kleisli <fun:f@4-14>
    
    > runKleisli f <| 5 ;;
    val it : int list = [8; 16; 24; 10; 20; 30]
    
    > (arr (fun y -> [y;y*2;y*3])) 3 ;;
    val it : int list = [3; 6; 9]
    
    > let (x:option<_>) = runKleisli (arr (fun y -> [y;y*2;y*3])) 2 ;;
    val x : int list option = Some [2; 4; 6]
    
    > ( (*) 100) *** ((+) 9)   <| (5,10) ;;
    val it : int * int = (500, 19)
    
    > ( (*) 100) &&& ((+) 9)   <| 5 ;;
    val it : int * int = (500, 14)
    
    > let x:List<_>  = (runKleisli (id'())) 5 ;;
    val x : List<int> = [5]
    

Note: use `id'()` instead of `id`

**Update:** you need F# 3.0 to compile this code, otherwise [here's the F# 2.0 version](http://code.google.com/p/fsharp-typeclasses/source/browse/Arrow.fs?r=a6389b6ea46ee96103d46bc71c1d7939f4aa9ff2).

And [here's](http://nut-cracker.azurewebsites.net/typeclasses-for-fsharp/) a detailed explanation of this technique which is type-safe, extensible and as you can see works even with some Higher Kind Typeclasses.
