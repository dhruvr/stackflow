
# New state of the art in unlimited generation of Hamming sequence

## Question
        
(this is exciting!) I know, the subject matter is well known. The state of the art (in Haskell as well as other languages) for efficient generation of unbounded increasing sequence of Hamming numbers, without duplicates and without omissions, has long been the following (AFAIK - and btw it is equivalent to the [original Edsger Dijkstra's code](http://web.cecs.pdx.edu/~cs410aph/Lectures/Smalltalk%20II/Dijkstra%20on%20Hamming%27s%20Problem.pdf) too):

    hamm :: [Integer]
    hamm = 1 : map (2*) hamm `union` map (3*) hamm `union` map (5*) hamm
      where
        union a@(x:xs) b@(y:ys) = case compare x y of
            LT -> x : union  xs  b
            EQ -> x : union  xs  ys
            GT -> y : union  a   ys
    

The question I'm asking is, **can you find the way to make it more efficient** in any significant measure? Is it still the state of the art or is it in fact possible to improve this to run _twice faster_ and with better [empirical orders of growth](http://en.wikipedia.org/wiki/Analysis_of_algorithms#Empirical_orders_of_growth) to boot?

If your answer is _yes_, please show the code and discuss its speed and empirical orders of growth in comparison to the above (it runs at about `~ n^1.05 .. n^1.10` for first few hundreds of thousands of numbers produced). Also, if it exists, can this efficient algorithm be extended to producing a sequence of smooth numbers with any given set of primes?

## Answer
        
If a constant factor(1) speedup counts as significant, then I can offer a significantly more efficient version:

    hamm :: [Integer]
    hamm = mrg1 hamm3 (map (2*) hamm)
      where
        hamm5 = iterate (5*) 1
        hamm3 = mrg1 hamm5 (map (3*) hamm3)
        merge a@(x:xs) b@(y:ys)
            | x < y     = x : merge xs b
            | otherwise = y : merge a ys
        mrg1 (x:xs) ys = x : merge xs ys
    

You can easily generalise it to smooth numbers for a given set of primes:

    hamm :: [Integer] -> [Integer]
    hamm [] = [1]
    hamm [p] = iterate (p*) 1
    hamm ps = foldl' next (iterate (q*) 1) qs
      where
        (q:qs) = sortBy (flip compare) ps
        next prev m = let res = mrg1 prev (map (m*) res) in res
        merge a@(x:xs) b@(y:ys)
            | x < y     = x : merge xs b
            | otherwise = y : merge a ys
        mrg1 (x:xs) ys = x : merge xs ys
    

It's more efficient because that algorithm doesn't produce any duplicates and it uses less memory. In your version, when a Hamming number near `h` is produced, the part of the list between `h/5` and `h` has to be in memory. In my version, only the part between `h/2` and `h` of the full list, and the part between `h/3` and `h` of the 3-5-list needs to be in memory. Since the 3-5-list is much sparser, and the density of k-smooth numbers decreases, those two list parts need much less memory that the larger part of the full list.

Some timings for the two algorithms to produce the `k`th Hamming number, with empirical complexity of each target relative to the previous, excluding and including GC time:

      k            Yours (MUT/GC)               Mine (MUT/GC)
     10^5           0.03/0.01                    0.01/0.01      -- too short to say much, really
    2*10^5          0.07/0.02                    0.02/0.01
    5*10^5          0.17/0.06  0.968  1.024      0.06/0.04      1.199    1.314
     10^6           0.36/0.13  1.082  1.091      0.11/0.10      0.874    1.070
    2*10^6          0.77/0.27  1.097  1.086      0.21/0.21      0.933    1.000
    5*10^6          1.96/0.71  1.020  1.029      0.55/0.59      1.051    1.090
     10^7           4.05/1.45  1.047  1.043      1.14/1.25      1.052    1.068
    2*10^7          8.73/2.99  1.108  1.091      2.31/2.65      1.019    1.053
    5*10^7         21.53/7.83  0.985  1.002      6.01/7.05      1.044    1.057
     10^8          45.83/16.79 1.090  1.093     12.42/15.26     1.047    1.084
    

As you can see, the factor between the MUT times is about 3.5, but the GC time is not much different.

(1) Well, it looks constant, and I think both variants have the same computational complexity, but I haven't pulled out pencil and paper to prove it, nor do I intend to.
