
# subsequences of length n from list performance

## Question
        
I implemented a version of this answer [https://stackoverflow.com/a/9920425/1261166](https://stackoverflow.com/a/9920425/1261166) (I don't know what was intended by the person answering)

    sublistofsize 0 _        = [[]]
    sublistofsize _ []       = []
    sublistofsize n (x : xs) = sublistsThatStartWithX ++ sublistsThatDontStartWithX
      where sublistsThatStartWithX = map (x:) $ sublistofsize (n-1) xs
            sublistsThatDontStartWithX = sublistofsize n xs
    

what I'm unsure of is `sublistsThatStartWithX = map (x:) $ sublistofsize (n-1) xs`

I assume that map (x:) gives a problem performance wise, but not sure of how to solve it. I've done profiling on `print $ length $ sublistofsize 5 $ primesToTakeFrom 50`

    COST CENTRE                                  MODULE                                        no.     entries  %time %alloc   %time %alloc
    sublistofsize                             Main                                          112     4739871   46.9   39.9    96.9  100.0
     sublistofsize.sublistsThatDontStartWithX Main                                          124     2369935    2.2    0.0     2.2    0.0
     sublistofsize.sublistsThatStartWithX     Main                                          116     2369935   47.8   60.1    47.8   60.1
    

Did I implement it in a good way? Are there any faster ways of doing it?

## Answer
        
> I assume that map (x:) gives a problem performance wise

No. `map` is coded efficiently and runs in linear time, no problems here.

However, your recursion might be a problem. You're both calling `sublistofsize (n-1) xs` and `sublistofsize n xs`, which - given a start list `sublistofsize m (_:_:ys)` \- does evaluate the term `sublistofsize (m-1) ys` twice, as there is no sharing between them in the different recursive steps.

So I'd apply dynamic programming to get

    subsequencesOfSize :: Int -> [a] -> [[a]]
    subsequencesOfSize n xs = let l = length xs
                              in if n>l then [] else subsequencesBySize xs !! (l-n)
     where
       subsequencesBySize [] = [[[]]]
       subsequencesBySize (x:xs) = let next = subsequencesBySize xs
                                 in zipWith (++) ([]:next) (map (map (x:)) next ++ [[]])
    

Not that appending the empty lists is the most beautiful solution, but you can see how I have used `zipWith` with the displaced lists so that the results from `next` are used twice - once directly in the list of subsequences of length n and once in the list of subsequences of length n+1.

Testing it in GHCI with `:set +s`, you can see how this is drastically faster than the naive solutions:

    *Main> length $ subsequencesOfSize 7 [1..25]
    480700
    (0.25 secs, 74132648 bytes)
    (0.28 secs, 73524928 bytes)
    (0.30 secs, 73529004 bytes)
    *Main> length $ sublistofsize 7 [1..25] -- @Vixen (question)
    480700
    (3.03 secs, 470779436 bytes)
    (3.35 secs, 470602932 bytes)
    (3.14 secs, 470747656 bytes)
    *Main> length $ sublistofsize' 7 [1..25] -- @Ganesh
    480700
    (2.00 secs, 193610388 bytes)
    (2.00 secs, 193681472 bytes)
    *Main> length $ subseq 7 [1..25] -- @user5402
    480700
    (3.07 secs, 485941092 bytes)
    (3.07 secs, 486279608 bytes)
