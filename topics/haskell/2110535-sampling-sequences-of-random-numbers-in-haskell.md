
# Sampling sequences of random numbers in Haskell

## Question
        
I need small lists of gaussian random numbers for a simulation and so I tried the following:

    import System.Random
    
    seed = 10101
    gen = mkStdGen seed
    
    boxMuller mu sigma (r1,r2) =  mu + sigma * sqrt (-2 * log r1) * cos (2 * pi * r2) 
    

This is just the Box-Muller algorithm - given r1, r2 uniform random numbers in the \[0,1\] interval it returns a gaussian random number.

    normals 0 g = [] 
    normals n g = take n $ map (boxMuller 0 1) $ pairs $ randoms g
        where pairs (x:y:zs) = (x,y):(pairs zs)
    

So I used this `normals` function everytime I needed my list of random numbers.

The problem with that must be apparent: it generates always the same sequence cause I'm using always the same seed! I'm not getting new sequences, I'm only getting the first n values of the sequence all the time.

What I was pretending clearly was that, when I type:

    x = normal 10 
    y = normal 50
    

I'd have x to be the first 10 values of `map (boxMuller 0 1) $ pairs $ randoms g` and y to be the next 50 values on this list, and so on.

Of course this is impossible, cause a function must always return the same values given the same input. How do I escape this trap?

## Answer
        
I think that doing your computations that require random numbers inside a monad that abstracts away the generator is the cleanest thing. Here is what that code would look like:

We are going to put the StdGen instance in a state monad, then provide some sugar over the state monad's get and set method to give us random numbers.

First, load the modules:

    import Control.Monad.State (State, evalState, get, put)
    import System.Random (StdGen, mkStdGen, random)
    import Control.Applicative ((<$>))
    

(Normally I would probably not specify the imports, but this makes it easy to understand where each function is coming from.)

Then we will define our "computations requiring random numbers" monad; in this case, an alias for `State StdGen` called `R`. (Because "Random" and "Rand" already mean something else.)

    type R a = State StdGen a
    

The way R works is: you define a computation that requires a stream of random numbers (the monadic "action"), and then you "run it" with `runRandom`:

    runRandom :: R a -> Int -> a
    runRandom action seed = evalState action $ mkStdGen seed
    

This takes an action and a seed, and returns the results of the action. Just like the usual `evalState`, `runReader`, etc.

Now we just need sugar around the State monad. We use `get` to get the StdGen, and we use `put` to install a new state. This means, to get one random number, we would write:

    rand :: R Double
    rand = do
      gen <- get
      let (r, gen') = random gen
      put gen'
      return r
    

We get the current state of the random number generator, use it to get a new random number and a new generator, save the random number, install the new generator state, and return the random number.

This is an "action" that can be run with runRandom, so let's try it:

    ghci> runRandom rand 42
    0.11040701265689151                           
    it :: Double     
    

This is a pure function, so if you run it again with the same arguments, you will get the same result. The impurity stays inside the "action" that you pass to runRandom.

Anyway, your function wants pairs of random numbers, so let's write an action to produce a _pair_ of random numbers:

    randPair :: R (Double, Double)
    randPair = do
      x <- rand
      y <- rand
      return (x,y)
    

Run this with runRandom, and you'll see two different numbers in the pair, as you'd expect. But notice that you didn't have to supply "rand" with an argument; that's because functions are pure, but "rand" is an action, which need not be pure.

Now we can implement your `normals` function. `boxMuller` is as you defined it above, I just added a type signature so I can understand what's going on without having to read the whole function:

    boxMuller :: Double -> Double -> (Double, Double) -> Double
    boxMuller mu sigma (r1,r2) =  mu + sigma * sqrt (-2 * log r1) * cos (2 * pi * r2)
    

With all the helper functions/actions implemented, we can finally write `normals`, an action of 0 arguments that returns a (lazily-generated) infinite list of normally-distributed Doubles:

    normals :: R [Double]
    normals = mapM (\_ -> boxMuller 0 1 <$> randPair) $ repeat ()
    

You could also write this less concisely if you want:

    oneNormal :: R Double
    oneNormal = do
        pair <- randPair
        return $ boxMuller 0 1 pair
    
    normals :: R [Double]
    normals = mapM (\_ -> oneNormal) $ repeat ()
    

`repeat ()` gives the monadic action an infinite stream of nothing to invoke the function with (and is what makes the result of normals infinitely long). I had initially written `[1..]`, but I rewrote it to remove the meaningless `1` from the program text. We are not operating on integers, we just want an infinite list.

Anyway, that's it. To use this in a real program, you just do your work requiring random normals inside an R action:

    someNormals :: Int -> R [Double]
    someNormals x = liftM (take x) normals
    
    myAlgorithm :: R [Bool]
    myAlgorithm = do
       xs <- someNormals 10
       ys <- someNormals 10
       let xys = zip xs ys
       return $ uncurry (<) <$> xys
    
    runRandom myAlgorithm 42
    

The usual techniques for programming monadic actions apply; keep as little of your application in `R` as possible, and things won't be too messy.

Oh, and on other thing: laziness can "leak" outside of the monad boundary cleanly. This means you can write:

    take 10 $ runRandom normals 42
    

and it will work.
