
# Why are side-effects modeled as monads in Haskell?

## Question
        
Could anyone give some pointers on why the impure computations in Haskell are modelled as monads?

I mean monad is just an interface with 4 operations, so what was the reasoning to modelling side-effects in it?

## Answer
        
Suppose a function has side effects. If we take all the effects it produces as the input and output parameters, then the function is pure to the outside world.

So for an impure function

    f' :: Int -> Int
    

we add the RealWorld to the consideration

    f :: Int -> RealWorld -> (Int, RealWorld)
    -- input some states of the whole world,
    -- modify the whole world because of the a side effects,
    -- then return the new world.
    

then `f` is pure again. We define a parametrized data type `IO a = RealWorld -> (a, RealWorld)`, so we don't need to type RealWorld so many times

    f :: Int -> IO Int
    

To the programmer, handling a RealWorld directly is too dangerous—in particular, if a programmer gets their hands on a value of type RealWorld, they might try to _copy_ it, which is basically impossible. (Think of trying to copy the entire filesystem, for example. Where would you put it?) Therefore, our definition of IO encapsulates the states of the whole world as well.

These impure functions are useless if we can't chain them together. Consider

    getLine :: IO String               = RealWorld -> (String, RealWorld)
    getContents :: String -> IO String = String -> RealWorld -> (String, RealWorld)
    putStrLn :: String -> IO ()        = String -> RealWorld -> ((), RealWorld)
    

We want to get a filename from the console, read that file, then print the content out. How would we do it if we can access the real world states?

    printFile :: RealWorld -> ((), RealWorld)
    printFile world0 = let (filename, world1) = getLine world0
                           (contents, world2) = (getContents filename) world1 
                       in  (putStrLn contents) world2 -- results in ((), world3)
    

We see a pattern here: the functions are called like this:

    ...
    (<result-of-f>, worldY) = f worldX
    (<result-of-g>, worldZ) = g <result-of-f> worldY
    ...
    

So we could define an operator `~~~` to bind them:

    (~~~) :: (IO b) -> (b -> IO c) -> IO c
    
    (~~~) ::      (RealWorld -> (b, RealWorld))
          -> (b -> RealWorld -> (c, RealWorld))
          ->       RealWorld -> (c, RealWorld)
    (f ~~~ g) worldX = let (resF, worldY) = f worldX in
                            g resF worldY
    

then we could simply write

    printFile = getLine ~~~ getContents ~~~ putStrLn
    

without touching the real world.

* * *

Now suppose we want to make the file content uppercase as well. Uppercasing is a pure function

    upperCase :: String -> String
    

But to make it into the real world, it has to return an `IO String`. It is easy to lift such a function:

    impureUpperCase :: String -> RealWorld -> (String, RealWorld)
    impureUpperCase str world = (upperCase str, world)
    

this can be generalized:

    impurify :: a -> IO a
    
    impurify :: a -> RealWorld -> (a, RealWorld)
    impurify a world = (a, world)
    

so that `impureUpperCase = impurify . upperCase`, and we can write

    printUpperCaseFile = 
        getLine ~~~ getContents ~~~ (impurify . upperCase) ~~~ putStrLn
    

(Note: Normally we write `getLine ~~~ getContents ~~~ (putStrLn . upperCase)`)

* * *

Now let's see what we've done:

1.  We defined an operator `(~~~) :: IO b -> (b -> IO c) -> IO c` which chains two impure functions together
2.  We defined a function `impurify :: a -> IO a` which converts a pure value to impure.

Now we make the identification `(>>=) = (~~~)` and `return = impurify`, and see? We've got a monad.

* * *

(To check whether it's really a monad there's few axioms should be satisfied:

(1) `return a >>= f = f a`

      impurify a               = (\world -> (a, world))
     (impurify a ~~~ f) worldX = let (resF, worldY) = (\world -> (a, world)) worldX 
                                 in f resF worldY
                               = let (resF, worldY) =            (a, worldX))       
                                 in f resF worldY
                               = f a worldX
    

(2) `f >>= return = f`

      (f ~~~ impurify) a worldX = let (resF, worldY) = impuify a worldX 
                                  in f resF worldY
                                = let (resF, worldY) = (a, worldX)     
                                  in f resF worldY
                                = f a worldX
    

(3) `f >>= (\x -> g x >>= h) = (f >>= g) >>= h`

Exercise.)
