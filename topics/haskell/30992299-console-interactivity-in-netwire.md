
# Console interactivity in Netwire?

## Question
        
I am testing with the `Netwire` haskell library and made it work with a simple `time` wire:

    import Control.Wire
    import Prelude hiding ((.), id)
    
    import Control.Monad.IO.Class
    import Data.Functor.Identity
    import System.IO
    
    wire :: (HasTime t s) => Wire s () m a t
    wire = time
    
    run :: (HasTime t s, MonadIO m, Show b, Show e) =>
           Session m s -> Wire s e m a b -> m ()
    run session wire = do
      (dt, session') <- stepSession session
      (wt', wire') <- stepWire wire dt $ Right undefined
      case wt' of
        -- | Exit
        Left _ -> return ()
        Right x -> do
          liftIO $ do
            putChar '\r'
            putStr $ either (\ex -> show ex) show wt'
            hFlush stdout
            -- Interactivity here?
            gotInput <- hReady stdin
            if gotInput then
              return ()
              else return ()
          run session' wire'
    
    main :: IO ()
    -- main = testWire clockSession_ wire
    main = run clockSession_ wire
    

Note: the `run` is basically modified from `testWire`, so I don't know if it is the correct way to form a network of wires. Part of the code origin from [http://todayincode.tumblr.com/post/96914679355/almost-a-netwire-5-tutorial](http://todayincode.tumblr.com/post/96914679355/almost-a-netwire-5-tutorial) but that tutorial does not say about events.

Now I am trying to add a bit interactivity to the program. For now, quit the program when any key is pressed. I suppose I should do some event switching. However, I am stuck here because I cannot find a way to either change `wire'` or switch the behaviour. I tried to read the API document and the source, but I don't see how to actually "fire" an Event or using it to switch the wire.

Again, since I am not yet very familiar with Haskell, I may have made some big stupid mistakes here.

**Update 1/2**

I got my goal working by the following code. The timer stops on any key press. _Update 2_ I managed to separate out `pollInput` into another `IO` only function, Yay!

    import Control.Wire
    import Prelude hiding ((.), id)
    
    import Control.Monad.IO.Class
    import Data.Functor.Identity
    import System.IO
    
    wire :: (HasTime t s) => Wire s () m a t
    wire = time
    
    run :: (HasTime t s, MonadIO m, Show b, Show e) =>
           Session m s -> Wire s e m a b -> m ()
    run session wire = do
      -- Get input here
      input <- liftIO $ pollInput
    
      (dt, session') <- stepSession session
      (wt', wire') <- stepWire wire dt $ input
      case wt' of
        -- | Exit
        Left _ -> liftIO (putStrLn "") >> return ()
        Right x -> do
          liftIO $ do
            putChar '\r'
            putStr $ either (\ex -> show ex) show wt'
            hFlush stdout
    
          run session' wire'
    
    pollInput :: IO (Either a b)
    pollInput =  do
      gotInput <- hReady stdin
      if gotInput then
        return (Left undefined)
        else return (Right undefined)
    
    
    setup :: IO ()
    setup = do
      hSetBuffering stdin NoBuffering
      hSetBuffering stdout NoBuffering
    
    
    main :: IO ()
    main = do
      setup
      run clockSession_ wire
    

However, this raises some further questions. First, is this good practise? Second, what is the type of `pollInput`? I tried to manually type it out but without success. Automatic type deduction works, though.

This is my explanation of how this code works:

First, the user input from console is polled, and after some logic, the "input" to wire is generated (poor name choice, but that input generated is the wire input) and passed along the network. Here, I simply pass an inhibition (`Left something`), and will cause the loop to exit. Of course, when exiting, the program produces a newline to make console look nicer.

(Well, I still don't understand how `Event` works, though)

**Update 3/4**

After reading @Cirdec 's answer, and fiddled a lot on my editor, I get this single threaded version without `IORef`, also quitting on pressing 'x'_Update 4_: (but it does not output anything):

    import Control.Wire
    import Prelude hiding ((.),id)
    import Control.Wire.Unsafe.Event
    import System.IO
    import Control.Monad.IO.Class
    
    data InputEvent = KeyPressed Char 
                    | NoKeyPressed
                    deriving (Ord, Eq, Read, Show)
    type OutputEvent = IO ()
    
    --- Wires
    example :: (HasTime t s, Monad m, Show t) =>
               Wire s () m (Event [InputEvent]) (Event [OutputEvent])
    example = switch $
              (fmap ((:[]) . print) <$> periodic 1 . time
               &&&
               fmap (const mkEmpty) <$> filterE (any (== KeyPressed 'x'))
               )
    
    readKeyboard :: IO (Either e (InputEvent))
    readKeyboard = do
      hSetBuffering stdin NoBuffering
      gotInput <- hReady stdin
      if gotInput then do
        c <- getChar
        return $ Right $ KeyPressed c
        else return $ Right $ NoKeyPressed
    
    output :: [OutputEvent] -> IO ()
    output (x:xs) = id x >> output xs
    output _ = return ()
    
    run :: (HasTime t s, MonadIO m) =>
           Session m s -> Wire s e m (Event [InputEvent]) (Event [OutputEvent]) -> m e
    run = go
      where
        go session wire = do
          -- | inputEvent :: Event InputEvent
          inputEvent <- liftIO $ readKeyboard
          (dt, session') <- stepSession session
          (wt', wire') <- stepWire wire dt (Event <$> (fmap (:[]) inputEvent))
          -- (wt', wire') <- stepWire wire dt (Right undefined)
          case wt' of
            Left a -> return a
            Right bEvent -> do
              case bEvent of
                Event b -> liftIO $ output b
                _ -> return ()
              go session' wire'
    
    main = do
      run clockSession_ example
    

I think this is much better than my original, but I am still not completely convinced whether it is good practise or not.

## Answer
        
First, I would point to [Kleisli Arrow in Netwire 5?](https://stackoverflow.com/questions/32745934/kleisli-arrow-in-netwire-5). I came up with that answer after a longggg time of trying to understand Monads and Arrows. I will put a minimal example using Kleisli Wire soon.

This program merely echos what the user types, and quits when it hits a `q`. Though useless, it demonstrates a probably good practice of using Netwire 5.

    mkKleisli :: (Monad m, Monoid e) => (a -> m b) -> Wire s e m a b
    mkKleisli f = mkGen_ $ \a -> liftM Right $ f a
    

This is the Kleisli wire constructor written in the answer in the post referenced. In summary, this function lifts any Kleisli function `a -> m b` into `Wire s e m a b`. This is the core about any I/O we are doing in this program.

Since we are echoing as user types, `hGetChar` is probably the best choice. Therefore, we lift that into a wire.

    inputWire :: Wire s () IO () Char
    inputWire = mkKleisli $ \_ -> hGetChar stdin
    

Similarly, we use the following wire to output characters on screen.

    outputWire :: Wire s () IO Char ()
    outputWire = mkKleisli $ putChar
    

Then to determine when we need to quit, a pure wire is constructed to output `True` when `q` is the input (Note that `mkSF_` can be used instead of `arr`).

    quitWire :: (Monad m, Monoid e) => Wire s e m Char Bool
    quitWire = arr $ quitNow
        where 
          quitNow c 
              | c == 'q' || c == 'Q' = True
              | otherwise = False
    

To actually use the information of quitting, we need to write a special (but really simple) `runWire` function which runs a wire of type `Wire s e m () Bool`. When the wire is inhibited or returns false, the function ends.

    runWire :: (Monad m) => Session m s -> Wire s e m () Bool -> m ()
    runWire s w = do
      (ds, s') <- stepSession s
      (quitNow, w') <- stepWire w ds (Right ())
      case quitNow of
        Right False -> runWire s' w'
        _ -> return ()
    

Now, let's put wires together.

    mainWire = inputWire >>> (quitWire &&& outputWire) >>> arr (\(q,_) -> q)
    

Of course we can use the Arrow syntax:

    mainWire = proc _ -> do 
      c <- inputWire -< ()
      q <- quitWire -< c
      outputWire -< c
      returnA -< q
    

Not sure if the `proc` version is faster or not, but in this simple example, both are quite readable.

We get input from `inputWire`, feed it to both `quitWire` and `outputWire` and get a tuple `(Bool, ())`. Then we take the first one as the final output.

At last, we run everything in `main`!

    main = do 
      hSetEcho stdin False 
      hSetBuffering stdin NoBuffering
      hSetBuffering stdout NoBuffering 
      runWire clockSession_ mainWire
    

Here comes the final code I used:

    {-# LANGUAGE Arrows #-}
    
    module Main where
    
    import Control.Wire
    import Control.Monad
    import Control.Arrow
    import System.IO
    import Prelude hiding ((.), id)
    
    mkKleisli :: (Monad m, Monoid e) => (a -> m b) -> Wire s e m a b
    mkKleisli f = mkGen_ $ \a -> liftM Right $ f a
    
    inputWire :: Wire s () IO () Char
    inputWire = mkKleisli $ \_ -> hGetChar stdin
    
    outputWire :: Wire s () IO Char ()
    outputWire = mkKleisli $ putChar
    
    quitWire :: (Monad m, Monoid e) => Wire s e m Char Bool
    quitWire = arr $ quitNow
        where 
          quitNow c 
              | c == 'q' || c == 'Q' = True
              | otherwise = False
    
    runWire :: (Monad m) => Session m s -> Wire s e m () Bool -> m ()
    runWire s w = do
      (ds, s') <- stepSession s
      (quitNow, w') <- stepWire w ds (Right ())
      case quitNow of
        Right False -> runWire s' w'
        _ -> return ()
    
    mainWire = inputWire >>> (quitWire &&& outputWire) >>> arr (\(q,_) -> q)
    
    main = do 
      hSetEcho stdin False 
      hSetBuffering stdin NoBuffering
      hSetBuffering stdout NoBuffering 
      runWire clockSession_ mainWire
