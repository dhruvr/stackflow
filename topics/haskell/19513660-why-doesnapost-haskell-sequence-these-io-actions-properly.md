
# Why doesn&apos;t Haskell sequence these IO actions properly?

## Question
        
A friend of mine asked me why was I learning Haskell. To demonstrate the power of Haskell I wrote a small program which displayed a list of prime numbers:

    main = do
        putStr "Enter the number of prime numbers to display: "
        number <- fmap read getLine :: IO Int
        print . take number . filter isPrime $ [2..]
    
    isPrime :: Integer -> Bool
    isPrime n = not . any ((== 0) . mod n) $ [2..floor . sqrt . fromInteger $ n]
    

The program works as expected save a minor anomaly. It prints the prompt message after taking an input number from the user resulting in an output like:

    12
    Enter the number of prime numbers to display: [2,3,5,7,11,13,17,19,23,29,31,37]
    

Why is Haskell not sequencing the IO actions correctly? Where am I going wrong?

## Answer
        
This looks more like a buffering than a sequencing problem. What platform are you on? Have you tried forcing unbuffered output?

    hSetBuffering stdout NoBuffering -- from System.IO
