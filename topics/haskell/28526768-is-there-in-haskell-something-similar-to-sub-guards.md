
# Is there, in Haskell, something similar to sub-guards?

## Question
        
I'm writing a program on the classification of musical intervals. The conceptual structure is quite complicated and I would represent it as clearly as possible. The first few lines of code are a small extract that works properly. The second are the pseudo-code that would meet my needs of conciseness.

    interval pt1 pt2
      | gd == 0 && sd <  (-2) = ("unison",show (abs sd) ++ "d") 
      | gd == 0 && sd == (-2) = ("unison","dd")
      | gd == 0 && sd == (-1) = ("unison","d")
      | gd == 0 && sd == 0    = ("unison","P")
      | gd == 0 && sd == 1    = ("unison","A")
      | gd == 0 && sd == 2    = ("unison","AA")
      | gd == 0 && sd >  2    = ("unison",show sd ++ "A")
    
      | gd == 1 && sd <  (-1) = ("second",show (abs sd) ++ "d")
      | gd == 1 && sd == (-1) = ("second","dd")
      | gd == 1 && sd == 0    = ("second","d")
      | gd == 1 && sd == 1    = ("second","m")
      | gd == 1 && sd == 2    = ("second","M")
      | gd == 1 && sd == 3    = ("second","A")
      | gd == 1 && sd == 4    = ("second","AA")
      | gd == 1 && sd >  4    = ("second",show (abs sd) ++ "A")
    
      where
      (bn1,acc1,oct1) = parsePitch pt1
      (bn2,acc2,oct2) = parsePitch pt2
      direction = signum sd
      sd = displacementInSemitonesOfPitches pt1 pt2
      gd = abs $ displacementBetweenTwoBaseNotes direction bn1 bn2
    

Is there a programming structure that could **simplify the code like the following pseudo-code does?**

    interval pt1 pt2 
      | gd == 0  | sd <  (-2) = ("unison",show (abs sd) ++ "d") 
                 | sd == (-2) = ("unison","dd")
                 | sd == (-1) = ("unison","d")
                 | sd == 0    = ("unison","P")
                 | sd == 1    = ("unison","A")
                 | sd == 2    = ("unison","AA")
                 | sd >  2    = ("unison",show sd ++ "A")  
      | gd == 1  | sd <  (-1) = ("second",show (abs sd) ++ "d")
                 | sd == (-1) = ("second","dd")
                 | sd == 0    = ("second","d")
                 | sd == 1    = ("second","m")
                 | sd == 2    = ("second","M")
                 | sd == 3    = ("second","A")
                 | sd == 4    = ("second","AA")
                 | sd >  4    = ("second",show (abs sd) ++ "A")
      | gd == 2  | sd ...     = ...
                 | sd ...     = ...
      ...
      | mod gd 7 == 1 | mod sd 12 == ...
                      | mod sd 12 == ...
      ...
      | otherwise = ...
    
      where
      (bn1,acc1,oct1) = parsePitch pt1
      (bn2,acc2,oct2) = parsePitch pt2
      direction = signum sd
      sd = displacementInSemitonesOfPitches pt1 pt2
      gd = abs $ displacementBetweenTwoBaseNotes direction bn1 bn2
    

Thank you in advance for your suggestions.

## Answer
        
Let me use a shorter example than the posted one:

    original :: Int -> Int
    original n
      | n < 10 && n > 7 = 1   -- matches 8,9
      | n < 12 && n > 5 = 2   -- matches 6,7,10,11
      | n < 12 && n > 3 = 3   -- matches 4,5
      | n < 13 && n > 0 = 4   -- matches 1,2,3,12
    

The code runs in GHCi as follows:

    > map original [1..12]
    [4,4,4,3,3,2,2,1,1,2,2,4]
    

Our aim is to "group" together the two branches requiring with `n < 12`, factoring this condition out. (This is not a huge gain in the `original` toy example, but it could be in more complex cases.)

We could naively think of splitting the code in two nested cases:

    wrong1 :: Int -> Int
    wrong1 n = case () of 
      _ | n < 10 && n > 7 -> 1
        | n < 12 -> case () of
                    _ | n > 5 -> 2
                      | n > 3 -> 3
        | n < 13 && n > 0 -> 4
    

Or, equivalently, using the `MultiWayIf` extension:

    wrong2 :: Int -> Int
    wrong2 n = if 
      | n < 10 && n > 7 -> 1
      | n < 12 -> if | n > 5 -> 2
                     | n > 3 -> 3
      | n < 13 && n > 0 -> 4
    

This however, leads to surprises:

    > map wrong1 [1..12]
    *** Exception: Non-exhaustive patterns in case
    
    > map wrong2 [1..12]
    *** Exception: Non-exhaustive guards in multi-way if
    

The issue is that when `n` is `1`, the `n < 12` branch is taken, the inner case is evaluated, and then no branch there considers `1`. The `original` code simply tries the next branch, which handles it. However, `wrong1,wrong2` are not backtracking to the outer case.

Please note that this is not a problem when you know that the outer case has non-overlapping conditions. In the code posted by the OP, this seems to be the case, so the `wrong1,wrong2` approaches would work there (as shown by Jefffrey).

However, what about the general case, where there might be overlaps? Fortunately, Haskell is lazy, so it's easy to roll our own control structures. For this, we can exploit the `Maybe` monad as follows:

    correct :: Int -> Int
    correct n = fromJust $ msum 
       [ guard (n < 10 && n > 7) >> return 1
       , guard (n < 12)          >> msum
          [ guard (n > 5) >> return 2
          , guard (n > 3) >> return 3 ]
       , guard (n < 13 && n > 0) >> return 4 ]
    

It _is_ a bit more verbose, but not by much. Writing code in this style is easier than it might look: a simple multiway conditional is written as

    foo n = fromJust $ msum 
       [ guard boolean1 >> return value1
       , guard boolean2 >> return value2
       , ...
       ]
    

and, if you want a "nested" case, just replace any of the `return value` with a `msum [ ... ]`.

Doing this ensures that we get the wanted backtracking. Indeed:

    > map correct [1..12]
    [4,4,4,3,3,2,2,1,1,2,2,4]
    

The trick here is that when a `guard` fails, it generates a `Nothing` value. The library function `msum` simply selects the first non-`Nothing` value in the list. So, even if every element in the inner list is `Nothing`, the outer `msum` will consider the next item in the outer list -- backtracking, as wanted.
