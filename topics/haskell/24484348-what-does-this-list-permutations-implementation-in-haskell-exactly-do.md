
# What does this list permutations implementation in Haskell exactly do?

## Question
        
I am studying the code in the Data.List module and can't exactly wrap my head around this implementation of permutations:

    permutations            :: [a] -> [[a]]
    permutations xs0        =  xs0 : perms xs0 []
      where
        perms []     _  = []
        perms (t:ts) is = foldr interleave (perms ts (t:is)) (permutations is)
          where interleave    xs     r = let (_,zs) = interleave' id xs r in zs
                interleave' _ []     r = (ts, r)
                interleave' f (y:ys) r = let (us,zs) = interleave' (f . (y:)) ys r
                                         in  (y:us, f (t:y:us) : zs)
    

Can somebody explain in detail how these nested functions connect/work with each other?

## Answer
        
Sorry about the late answer, it took a bit longer to write down than expected.

* * *

So, first of all to maximize lazyness in a list function like this there are two goals:

*   Produce as many answers as possible before inspecting the next element of the input list
*   The answers themselves must be lazy, and so there the same must hold.

Now consider the `permutation` function. Here maximal lazyness means:

*   We should determine that there are at least `n!` permutations after inspecting just `n` elements of input
*   For each of these `n!` permutations, the first `n` elements should depend only on the first `n` elements of the input.

The first condition could be formalized as

    length (take (factorial n) $ permutations ([1..n] ++ undefined))) `seq` () == ()
    

David Benbennick formalized the second condition as

    map (take n) (take (factorial n) $ permutations [1..]) == permutations [1..n] 
    

Combined, we have

    map (take n) (take (factorial n) $ permutations ([1..n] ++ undefined)) == permutations [1..n] 
    

Let's start with some simple cases. First `permutation [1..]`. We must have

    permutations [1..] = [1,???] : ???
    

And with two elements we must have

    permutations [1..] = [1,2,???] : [2,1,???] : ???
    

Note that there is no choice about the order of the first two elements, we can't put `[2,1,...]` first, since we already decided that the first permutation must start with `1`. It should be clear by now that the first element of `permutations xs` must be equal to `xs` itself.

* * *

Now on to the implementation.

First of all, there are two different ways to make all permutations of a list:

1.  Selection style: keep picking elements from the list until there are none left
    
        permutations []  = [[]]
        permutations xxs = [(y:ys) | (y,xs) <- picks xxs, ys <- permutations xs]
          where
            picks (x:xs) = (x,xs) : [(y,x:ys) | (y,ys) <- picks xs]
        
    
2.  Insertion style: insert or interleave each element in all possible places
    
        permutations []     = [[]]
        permutations (x:xs) = [y | p <- permutations xs, y <- interleave p]
          where
            interleave []     = [[x]]
            interleave (y:ys) = (x:y:ys) : map (y:) (interleave ys)
        
    

Note that neither of these is maximally lazy. The first case, the first thing this function does is pick the first element from the entire list, which is not lazy at all. In the second case we need the permutations of the tail before we can make any permutation.

To start, note that `interleave` can be made more lazy. The first element of `interleave yss` list is `[x]` if `yss=[]` or `(x:y:ys)` if `yss=y:ys`. But both of these are the same as `x:yss`, so we can write

    interleave yss = (x:yss) : interleave' yss
    interleave' [] = []
    interleave' (y:ys) = map (y:) (interleave ys)
    

The implementation in Data.List continues on this idea, but uses a few more tricks.

It is perhaps easiest to go through the [mailing list discussion](http://haskell.1045720.n5.nabble.com/Add-subsequences-and-permutations-to-Data-List-ticket-1990-td3173688.html). We start with David Benbennick's version, which is the same as the one I wrote above (without the lazy interleave). We already know that the first elment of `permutations xs` should be `xs` itself. So, let's put that in

    permutations xxs     = xxs : permutations' xxs
    permutations' []     = []
    permutations' (x:xs) = tail $ concatMap interleave $ permutations xs
      where interleave = ..
    

The call to `tail` is of course not very nice. But if we inline the definitions of `permutations` and `interleave` we get

    permutations' (x:xs)
      = tail $ concatMap interleave $ permutations xs
      = tail $ interleave xs ++ concatMap interleave (permutations' xs)
      = tail $ (x:xs) : interleave' xs ++ concatMap interleave (permutations' xs)
      = interleave' xs ++ concatMap interleave (permutations' xs)
    

Now we have

    permutations xxs     = xxs : permutations' xxs
    permutations' []     = []
    permutations' (x:xs) = interleave' xs ++ concatMap interleave (permutations' xs)
      where
       interleave yss = (x:yss) : interleave' yss
       interleave' [] = []
       interleave' (y:ys) = map (y:) (interleave ys)
    

The next step is optimization. An important target would be to eliminate the (++) calls in interleave. This is not so easy, because of the last line, `map (y:) (interleave ys)`. We can't immediately use the foldr/ShowS trick of passing the tail as a parameter. The way out is to get rid of the map. If we pass a parameter `f` as the function that has to be mapped over the result at the end, we get

    permutations' (x:xs) = interleave' id xs ++ concatMap (interleave id) (permutations' xs)
      where
       interleave f yss = f (x:yss) : interleave' f yss
       interleave' f [] = []
       interleave' f (y:ys) = interleave (f . (y:)) ys
    

Now we can pass in the tail,

    permutations' (x:xs) = interleave' id xs $ foldr (interleave id) [] (permutations' xs)
      where
       interleave  f yss    r = f (x:yss) : interleave' f yss r
       interleave' f []     r = r
       interleave' f (y:ys) r = interleave (f . (y:)) ys r
    

This is starting to look like the one in Data.List, but it is not the same yet. In particular, it is not as lazy as it could be. Let's try it out:

    *Main> let n = 4
    *Main> map (take n) (take (factorial n) $ permutations ([1..n] ++ undefined))
    [[1,2,3,4],[2,1,3,4],[2,3,1,4],[2,3,4,1]*** Exception: Prelude.undefined
    

Uh oh, only the first `n` elements are correct, not the first `factorial n`. The reason is that we still try to place the first element (the `1` in the above example) in all possible locations before trying anything else.

* * *

Yitzchak Gale came up with a solution. Considered all ways to split the input into an initial part, a middle element, and a tail:

    [1..n] == []    ++ 1 : [2..n]
           == [1]   ++ 2 : [3..n]
           == [1,2] ++ 3 : [4..n]
    

If you haven't seen the trick to generate these before before, you can do this with `zip (inits xs) (tails xs)`. Now the permutations of `[1..n]` will be

*   `[] ++ 1 : [2..n]` aka. `[1..n]`, or
*   `2` inserted (interleaved) somewhere into a permutation of `[1]`, followed by `[3..n]`. But not `2` inserted at the end of `[1]`, since we already go that result in the previous bullet point.
*   `3` interleaved into a permutation of `[1,2]` (not at the end), followed by `[4..n]`.
*   etc.

You can see that this is maximally lazy, since before we even consider doing something with `3`, we have given all permutations that start with some permutation of `[1,2]`. The code that Yitzchak gave was

    permutations xs = xs : concat (zipWith newPerms (init $ tail $ tails xs)
                                                    (init $ tail $ inits xs))
      where
        newPerms (t:ts) = map (++ts) . concatMap (interleave t) . permutations3
        interleave t [y]        = [[t, y]]
        interleave t ys@(y:ys') = (t:ys) : map (y:) (interleave t ys') 
    

Note the recursive call to `permutations3`, which can be a variant that doesn't have to be maximally lazy.

As you can see this is a bit less optimized than what we had before. But we can apply some of the same tricks.

The first step is to get rid of `init` and `tail`. Let's look at what `zip (init $ tail $ tails xs) (init $ tail $ inits xs)` actually is

    *Main> let xs = [1..5] in zip (init $ tail $ tails xs) (init $ tail $ inits xs)
    [([2,3,4,5],[1]),([3,4,5],[1,2]),([4,5],[1,2,3]),([5],[1,2,3,4])]
    

The `init` gets rid of the combination `([],[1..n])`, while the `tail` gets rid of the combination `([1..n],[])`. We don't want the former, because that would fail the pattern match in `newPerms`. The latter would fail `interleave`. Both are easy to fix: just add a case for `newPerms []` and for `interleave t []`.

    permutations xs = xs : concat (zipWith newPerms (tails xs) (inits xs))
      where
        newPerms [] is = []
        newPerms (t:ts) is = map (++ts) (concatMap (interleave t) (permutations is))
        interleave t []         = []
        interleave t ys@(y:ys') = (t:ys) : map (y:) (interleave t ys') 
    

Now we can try to inline `tails` and `inits`. Their definition is

    tails xxs = xxs : case xxs of
      []     -> []
      (_:xs) -> tails xs
    
    inits xxs = [] : case xxs of
      []     -> []
      (x:xs) -> map (x:) (inits xs)
    

The problem is that `inits` is not tail recursive. But since we are going to take a permutation of the inits anyway, we don't care about the order of the elements. So we can use an accumulating parameter,

    inits' = inits'' []
      where
      inits'' is xxs = is : case xxs of
        []     -> []
        (x:xs) -> inits'' (x:is) xs
    

Now we make `newPerms` a function of `xxs` and this accumulating parameter, instead of `tails xxs` and `inits xxs`.

    permutations xs = xs : concat (newPerms' xs [])
      where
        newPerms' xxs is =
          newPerms xxs is :
          case xxs of
            []     -> []
            (x:xs) -> newPerms' xs (x:is)
        newPerms [] is = []
        newPerms (t:ts) is = map (++ts) (concatMap (interleave t) (permutations3 is))
    

inlining `newPerms` into `newPerms'` then gives

    permutations xs = xs : concat (newPerms' xs [])
      where
        newPerms' []     is = [] : []
        newPerms' (t:ts) is =
          map (++ts) (concatMap (interleave t) (permutations is)) :
          newPerms' ts (t:is)
    

inlining and unfolding `concat`, and moving the final `map (++ts)` into `interleave`,

    permutations xs = xs : newPerms' xs []
      where
        newPerms' []     is = []
        newPerms' (t:ts) is =
            concatMap interleave (permutations is) ++
            newPerms' ts (t:is)
          where
          interleave []     = []
          interleave (y:ys) = (t:y:ys++ts) : map (y:) (interleave ys) 
    

Then finally, we can reapply the `foldr` trick to get rid of the `(++)`:

    permutations xs = xs : newPerms' xs []
      where
        newPerms' []     is = []
        newPerms' (t:ts) is =
            foldr (interleave id) (newPerms' ts (t:is)) (permutations is)
          where
          interleave f []     r = r
          interleave f (y:ys) r = f (t:y:ys++ts) : interleave (f . (y:)) ys r
    

Wait, I said get rid of the `(++)`. We got rid of one of them, but not the one in `interleave`. For that, we can see that we are always concatenating some tail of `yys` to `ts`. So, we can unfold the calculating `(ys++ts)` along with the recursion of `interleave`, and have the function `interleave' f ys r` return the tuple `(ys++ts, interleave f ys r)`. This gives

    permutations xs = xs : newPerms' xs []
      where
        newPerms' []     is = []
        newPerms' (t:ts) is =
            foldr interleave (newPerms' ts (t:is)) (permutations is)
          where
          interleave ys r = let (_,zs) = interleave' id ys r in zs
          interleave' f []     r = (ts,r)
          interleave' f (y:ys) r = 
            let (us,zs) = interleave' (f . (y:)) ys r
            in  (y:us, f (t:y:us) : zs)
    

And there you have it, `Data.List.permutations` in all its maximally lazy optimized glory.

* * *

Great write-up by Twan! I (@Yitz) will just add a few references:

*   The original email thread where Twan developed this algorithm, linked above by Twan, is fascinating reading.
    
*   Knuth classifies all possible algorithms that satisfy these criteria in Vol. 4 Fasc. 2 Sec. 7.2.1.2.
    
*   Twan's `permutations3` is essentially the same as Knuth's "Algorithm P". As far as Knuth knows, that algorithm was first published by English church bell ringers in the 1600's.
