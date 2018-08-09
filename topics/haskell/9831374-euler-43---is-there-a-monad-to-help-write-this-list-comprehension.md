
# Euler 43 - is there a monad to help write this list comprehension?

## Question
        
Here is a way to solve Euler problem 43 (please let me know if this doesn't give the correct answer). Is there a monad or some other syntatic sugar which could assist with keeping track of the `notElem` conditions?

    toNum xs = foldl (\s d -> s*10+d) 0 xs
    
    numTest xs m = (toNum xs) `mod` m == 0
    
    pandigitals = [ [d0,d1,d2,d3,d4,d5,d6,d7,d8,d9] |
                    d7 <- [0..9],
                    d8 <- [0..9], d8 `notElem` [d7],
                    d9 <- [0..9], d9 `notElem` [d8,d7],
                    numTest [d7,d8,d9] 17,
                    d5 <- [0,5],  d5 `notElem` [d9,d8,d7],
                    d3 <- [0,2,4,6,8], d3 `notElem` [d5,d9,d8,d7],
                    d6 <- [0..9], d6 `notElem` [d3,d5,d9,d8,d7],
                    numTest [d6,d7,d8] 13,
                    numTest [d5,d6,d7] 11,
                    d4 <- [0..9], d4 `notElem` [d6,d3,d5,d9,d8,d7],
                    numTest [d4,d5,d6] 7,
                    d2 <- [0..9], d2 `notElem` [d4,d6,d3,d5,d9,d8,d7],
                    numTest [d2,d3,d4] 3,
                    d1 <- [0..9], d1 `notElem` [d2,d4,d6,d3,d5,d9,d8,d7],
                    d0 <- [1..9], d0 `notElem` [d1,d2,d4,d6,d3,d5,d9,d8,d7]
                  ]
    
    main = do
             let nums = map toNum pandigitals
             print $ nums
             putStrLn ""
             print $ sum nums
    

For instance, in this case the assignment to `d3` is not optimal - it really should be moved to just before the `numTest [d2,d3,d4] 3` test. Doing that, however, would mean changing some of the `notElem` tests to remove `d3` from the list being checked. Since the successive `notElem` lists are obtained by just consing the last chosen value to the previous list, it seems like this should be doable - somehow.

UPDATE: Here is the above program re-written with Louis' `UniqueSel` monad below:

    toNum xs = foldl (\s d -> s*10+d) 0 xs
    numTest xs m = (toNum xs) `mod` m == 0
    
    pandigitalUS =
      do d7 <- choose
         d8 <- choose
         d9 <- choose
         guard $ numTest [d7,d8,d9] 17
         d6 <- choose
         guard $ numTest [d6,d7,d8] 13
         d5 <- choose
         guard $ d5 == 0 || d5 == 5
         guard $ numTest [d5,d6,d7] 11
         d4 <- choose
         guard $ numTest [d4,d5,d6] 7
         d3 <- choose
         d2 <- choose
         guard $ numTest [d2,d3,d4] 3
         d1 <- choose
         guard $ numTest [d1,d2,d3] 2
         d0 <- choose
         guard $ d0 /= 0
         return [d0,d1,d2,d3,d4,d5,d6,d7,d8,d9]
    
    pandigitals = map snd $ runUS pandigitalUS [0..9]
    
    main = do print $ pandigitals

## Answer
        
Sure.

    newtype UniqueSel a = UniqueSel {runUS :: [Int] -> [([Int], a)]}
    instance Monad UniqueSel where
      return a = UniqueSel (\ choices -> [(choices, a)])
      m >>= k = UniqueSel (\ choices -> 
        concatMap (\ (choices', a) -> runUS (k a) choices')
          (runUS m choices))
    
    instance MonadPlus UniqueSel where
      mzero = UniqueSel $ \ _ -> []
      UniqueSel m `mplus` UniqueSel k = UniqueSel $ \ choices ->
        m choices ++ k choices
    
    -- choose something that hasn't been chosen before
    choose :: UniqueSel Int
    choose = UniqueSel $ \ choices ->
      [(pre ++ suc, x) | (pre, x:suc) <- zip (inits choices) (tails choices)]
    

and then you treat it like the List monad, with `guard` to enforce choices, except that it won't choose an item more than once. Once you have a `UniqueSel [Int]` computation, just do `map snd (runUS computation [0..9])` to give it `[0..9]` as the choices to select from.
