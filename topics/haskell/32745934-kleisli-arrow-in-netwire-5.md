
# Kleisli Arrow in Netwire 5?

## Question
        
I am trying to create a game using Haskell + Netwire 5 (+ SDL). Now I am working on the output part, where I would like to create wires that read in some game state and output the SDL surfaces to be blitted on screen.

However, the problem is that SDL surfaces are contained in `IO` monad, so any function that creates such surfaces must have type `a -> IO b`. Of course, `arr` does not construct a `Wire` from `a -> m b`. However, since the type signature of a wire is `(Monad m, Monoid e) => Wire s e m a b`, it looks quite like a Kleisi Arrow, but I cannot find a suitable constructor for making such a wire.

I am new to FRP and Arrows, and have not programmed a lot in Haskell, so this may not be the best way to implement the graphics output. If I am wrong from the beginning, please let me know.

Some SDL functions related:

    createRGBSurfaceEndian :: [SurfaceFlag] -> Int -> Int -> Int -> IO Surface
    
    fillRect :: Surface -> Maybe Rect -> Pixel -> IO Bool
    
    blitSurface :: Surface -> Maybe Rect -> Surface -> Maybe Rect -> IO Bool
    
    flip :: Surface -> IO ()
    

Update 1
========

This code type checks, but now I am trying to interface it with SDL for testing

    wTestOutput :: (Monoid e) => Wire s e IO () SDL.Surface
    wTestOutput = mkGen_ $ \a -> (makeSurf a >>= return . Right)
        where
          makeSurf :: a -> IO SDL.Surface
          makeSurf _ = do
            s <- SDL.createRGBSurfaceEndian [SDL.SWSurface] 800 600 32
            SDL.fillRect s (Just testRect) (SDL.Pixel 0xFF000000)
            return s
          testRect = SDL.Rect 100 100 0 0

## Answer
        
Now, after playing around with Arrows, I will answer my own question using the function `putStrLn`. It has type `String -> IO ()`, which is `a -> m b`, so the method should generalize to all Kleisli wires. I also illustrate how to drive the wire, and the result is amazingly simple.

The entire code is written in Literate Haskell, so just copy it and run.

First, there are some imports for the Netwire 5 library

    import Control.Wire
    import Control.Arrow
    import Prelude hiding ((.), id)
    

Now, this is the core of making a Kleisli Wire. Assume you have a function with type `a -> m b` that needs to be lifted into a wire. Now, notice that `mkGen_` has type `mkGen_ :: Monad m => (a -> m (Either e b)) -> Wire s e m a b`

So, to make a wire out of `a -> m b`, we first need to get a function with type `a -> m (Either () b)`. Notice that Left inhibits the wire, while Right activates it, so the inner part is `Either () b` instead of `Either b ()`. Actually, if you try the latter, an obscure compile error will tell you get this in the wrong way.

To get `a -> m (Either () b)`, first consider how to get `m (Either () b)` from `m b`, we extract the value from the monad (m b), lift it to Right, then return to the monad m. In short: `mB >>= return . Right`. Since we don't have the value "mB" here, we make a lambda expression to get `a -> m (Either () b)`:

    liftToEither :: (Monad m) => (a -> m b) -> (a -> m (Either () b))
    liftToEither f = \a -> (f a >>= return . Right)
    

Now, we can make a Kleisli wire:

    mkKleisli :: (Monad m, Monoid e) => (a -> m b) -> Wire s e m a b
    mkKleisli f = mkGen_ $ \a -> (f a >>= return . Right)
    

So, let's try the canonical "hello, world" wire!

    helloWire :: Wire s () IO () ()
    helloWire = pure "hello, world" >>> mkKleisli putStrLn
    

Now comes the main function to illustrate how to drive the wire. Note that comparing to the source of `testWire` in `the Control.Wire.Run` from the Netwire library, there is no use of liftIO: the outer program knows nothing about how the wires work internally. It merely steps the wires ignoring what is in it. `Maybe` this `Just` means better composition than using `Nothing` about Kleisli Wires? (No pun intended!)

    main = go clockSession_ helloWire
        where
          go s w = do
            (ds, s') <- stepSession s
            (mx, w') <- stepWire w ds (Right ())
            go s' w'
    

Now here comes the code. Unfortunately StackOverflow does not work quite well with Literate Haskell...

    {-# LANGUAGE Arrows #-}
    
    module Main where
    
    import Control.Wire
    import Control.Monad
    import Control.Arrow
    import Prelude hiding ((.), id)
    
    mkKleisli :: (Monad m, Monoid e) => (a -> m b) -> Wire s e m a b
    mkKleisli f = mkGen_ $ \a -> liftM Right $ f a
    
    helloWire :: Wire s () IO () ()
    helloWire = pure "hello, world" >>> mkKleisli putStrLn
    
    main = go clockSession_ helloWire
        where
          go s w = do
            (ds, s') <- stepSession s
            (mx, w') <- stepWire w ds (Right ())
            go s' w'
    

_Update_

Thanks to Cubic's inspiration. `liftToEither` can actually be written in, you guess it, `liftM`:

    liftToEither f = \a -> liftM Right $ f a
    mkKleisli f = mkGen_ $ \a -> liftM Right $ f a
