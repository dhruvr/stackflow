
# Why recursive `let` make space effcient?

## Question
        
I found this statement while studying Functional Reactive Programming, from ["Plugging a Space Leak with an Arrow"](http://cs-www.cs.yale.edu/homes/hl293/download/leak.pdf) by Hai Liu and Paul Hudak ( page 5) :

>     Suppose we wish to deﬁne a function that repeats its argument indeﬁnitely:
>     
>         repeat x = x : repeat x
>     
>     or, in lambdas:
>     
>         repeat = λx → x : repeat x
>     
>     This requires O(n) space. But we can achieve O(1) space by writing instead:
>     
>         repeat = λx → let xs = x : xs
>                       in xs
>     

The difference here seems small but it hugely prompts the space efficiency. Why and how it happens ? The best guess I've made is to evaluate them by hand:

        r = \x -> x: r x
        r 3
    
        -> 3: r 3 
        -> 3: 3: 3: ........
        -> [3,3,3,......]
    

As above, we will need to create infinite new thunks for these recursion. Then I try to evaluate the second one:

        r = \x -> let xs = x:xs in xs
        r 3
    
        -> let xs = 3:xs in xs
        -> xs, according to the definition above: 
        -> 3:xs, where xs = 3:xs
        -> 3:xs:xs, where xs = 3:xs
    

In the second form the `xs` appears and can be shared between every places it occurring, so I guess that's why we can only require `O(1)` spaces rather than `O(n)`. But I'm not sure whether I'm right or not.

BTW: The keyword "shared" comes from the same paper's page 4:

> The problem here is that the standard call-by-need evaluation rules are unable to recognize that the function:
> 
>     f = λdt → integralC (1 + dt) (f dt) 
>     
> 
> is the same as:
> 
>     f = λdt → let x = integralC (1 + dt) x in x
>     
> 
> The former deﬁnition causes work to be repeated in the recursive call to f, whereas in the latter case the computation is shared.

## Answer
        
It's easiest to understand with pictures:

*   The first version
    
        repeat x = x : repeat x
        
    
    creates a chain of `(:)` constructors ending in a thunk which will replace itself with more constructors as you demand them. Thus, O(n) space.
    
    ![a chain](https://i.stack.imgur.com/Otq3Y.png)
    
*   The second version
    
        repeat x = let xs = x : xs in xs
        
    
    uses `let` to "tie the knot", creating a single `(:)` constructor which refers to itself.
    
    ![a loop](https://i.stack.imgur.com/TM5NB.png)
