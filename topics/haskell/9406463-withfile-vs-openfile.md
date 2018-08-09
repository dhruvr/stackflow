
# withFile vs. openFile

## Question
        
This program produces the output I expect when given an input file of text delimited by \\n:

    import System.IO
    
    main :: IO ()
    main = do h <- openFile "test.txt" ReadMode 
              xs <- getlines h
              sequence_ $ map putStrLn xs
    
    getlines :: Handle -> IO [String]
    getlines h = hGetContents h >>= return . lines
    

By substituting withFile for openFile and rearranging slightly

    import System.IO
    
    main :: IO ()
    main = do xs <- withFile "test.txt" ReadMode getlines
              sequence_ $ map putStrLn xs
    
    getlines :: Handle -> IO [String]
    getlines h = hGetContents h >>= return . lines  
    

I manage to get no output at all. I'm stumped.

Edit: Not stumped anymore: thanks to one and all for the thoughtful and thought-provoking answers. I did a little more reading in the documentation and learned that **withFile** can be understood as a partial application of **bracket**.

This is what I ended up with:

    import System.IO
    
    main :: IO ()
    main = withFile "test.txt" ReadMode $ \h -> getlines h >>= mapM_ putStrLn 
    
    getlines :: Handle -> IO [String]
    getlines h = lines `fmap` hGetContents h

## Answer
        
The file is being closed too early. From the [documentation](http://hackage.haskell.org/packages/archive/base/latest/doc/html/System-IO.html#v:withFile):

> The handle will be **closed on exit from withFile**

This means the file will be closed as soon as the `withFile` function returns.

Because `hGetContents` and friends are lazy, it won't try to read the file until it is forced with `putStrLn`, but by then, `withFile` would have closed the file already.

To solve the problem, pass the whole thing to `withFile`:

    main = withFile "test.txt" ReadMode $ \handle -> do
               xs <- getlines handle
               sequence_ $ map putStrLn xs
    

This works because by the time `withFile` gets around to closing the file, you would have already printed it.
