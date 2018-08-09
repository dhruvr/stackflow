
# Lazy Evaluation and Time Complexity

## Question
        
I was looking around stackoverflow [Non-Trivial Lazy Evaluation](https://stackoverflow.com/questions/7868507/non-trivial-lazy-evaluation), which led me to Keegan McAllister's presentation: [Why learn Haskell](http://ugcs.net/~keegan/talks/why-learn-haskell/talk.pdf). In slide 8, he shows the minimum function, defined as:

    minimum = head . sort
    

and states that its complexity is O(n). I don't understand why the complexity is said to be linear if sorting by replacement is O(nlog n). The sorting referred in the post can't be linear, as it does not assume anything about the data, as it would be required by linear sorting methods, such as counting sort.

Is lazy evaluation playing a mysterious role in here? If so, what is the explanation behind it?

## Answer
        
In `minimum = head . sort`, the `sort` won't be done fully, because it won't be done _upfront_. The `sort` will only be done as much as needed to produce the very first element, demanded by `head`.

In e.g. mergesort, at first `n` numbers of the list will be compared pairwise, then the winners will be paired up and compared (`n/2` numbers), then the new winners (`n/4`), etc. In all, `O(n)` comparisons to produce the minimal element.

    mergesortBy less [] = []
    mergesortBy less xs = head $ until (null.tail) pairs [[x] | x <- xs]
      where
        pairs (x:y:t) = merge x y : pairs t
        pairs xs      = xs
        merge (x:xs) (y:ys) | less y x  = y : merge (x:xs) ys
                            | otherwise = x : merge  xs (y:ys)
        merge  xs     []                = xs
        merge  []     ys                = ys
    

* * *

The above code can be augmented to tag each number it produces with a number of comparisons that went into its production:

    mgsort xs = go $ map ((,) 0) xs  where
      go [] = []
      go xs = head $ until (null.tail) pairs [[x] | x <- xs]   where
        ....
        merge ((a,b):xs) ((c,d):ys) 
                | (d < b)   = (a+c+1,d) : merge ((a+1,b):xs) ys    -- cumulative
                | otherwise = (a+c+1,b) : merge  xs ((c+1,d):ys)   --   cost
        ....
    
    g n = concat [[a,b] | (a,b) <- zip [1,3..n] [n,n-2..1]]   -- a little scrambler
    

Running it for several list lengths we see that **_it is indeed `~ n`_**:

    *Main> map (fst . head . mgsort . g) [10, 20, 40, 80, 160, 1600]
    [9,19,39,79,159,1599]
    

* * *

To see whether the sorting code itself is `~ n log n`, we change it so that each produced number carries along just its own cost, and the total cost is then found by summation over the whole sorted list:

        merge ((a,b):xs) ((c,d):ys) 
                | (d < b)   = (c+1,d) : merge ((a+1,b):xs) ys      -- individual
                | otherwise = (a+1,b) : merge  xs ((c+1,d):ys)     --   cost
    

Here are the results for lists of various lengths,

    *Main> let xs = map (sum . map fst . mgsort . g) [20, 40, 80, 160, 320, 640]
    [138,342,810,1866,4218,9402]
    
    *Main> map (logBase 2) $ zipWith (/) (tail xs) xs
    [1.309328,1.2439256,1.2039552,1.1766101,1.1564085]
    

The above shows [**empirical orders of growth**](http://en.wikipedia.org/wiki/Analysis_of_algorithms#Empirical_orders_of_growth) for increasing lengths of list, `n`, which are rapidly diminishing as is typically exhibited by **`~ n log n`** computations. See also [this blog post](http://rjlipton.wordpress.com/2009/07/24/how-to-avoid-o-abuse-and-bribes/). Here's a quick correlation check:

    *Main> let xs = [n*log n | n<- [20, 40, 80, 160, 320, 640]] in 
                                        map (logBase 2) $ zipWith (/) (tail xs) xs
    [1.3002739,1.2484156,1.211859,1.1846942,1.1637106]
    

* * *

_edit:_ Lazy evaluation can metaphorically be seen as kind of producer/consumer idiom1, with independent memoizing storage as an intermediary. Any productive definition we write, defines a producer which will produce its output, bit by bit, as and when demanded by its consumer(s) - but not sooner. Whatever is produced is memoized, so that if another consumer consumes same output at different pace, it accesses same storage, filled previously.

When no more consumers remain that refer to a piece of storage, it gets garbage collected. Sometimes with optimizations compiler is able to do away with the intermediate storage completely, cutting the middle man out.

1 see also: [Simple Generators v. Lazy Evaluation](http://lambda-the-ultimate.org/node/4690) by Oleg Kiselyov, Simon Peyton-Jones and Amr Sabry.
