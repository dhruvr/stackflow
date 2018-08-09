
# &#x201C;Couldn&apos;t match expected type with actual type&#x201D; error when using Codec.BMP

## Question
        
I have a very little experience in Haskell and I want to write a simple ray tracer for practice. Because I didn't want to use GUI tools like wxHaskell (I think it'll take a lot of time to learn how to use them), I decided to simply save the output image to BMP file. But I have a problem here:

    module Main where
    
    import Codec.BMP
    import qualified Data.ByteString as BS
    
    main = do
      Right bmp <- readBMP "grass.bmp"
      BS.putStrLn $ BS.take 4 $ unpackBMPToRGBA32 bmp
    

Here I just want to take first pixel of the image and print its RGBA values. But I get an error saying

    Couldn't match expected type `BS.ByteString'
                with actual type `bytestring-0.9.2.1:Data.ByteString.Internal.ByteString'
    In the return type of a call of `unpackBMPToRGBA32'
    In the second argument of `($)', namely `unpackBMPToRGBA32 bmp'
    In the second argument of `($)', namely
      `BS.take 4 $ unpackBMPToRGBA32 bmp'
    

What am I doing wrong? How can I take the pixels of the image and print their values?

## Answer
        
You have two `bytestring` packages installed, so `unpackBMPToRGBA32` return `ByteString` from `bytestring-0.9.2.1`, and `BS.putStrLn` expects `ByteString` from other version.

Try `ghc-pkg list bytestring` to list all `bytestring` versions installed.

And the solution could be

*   unregister one of them with `ghc-pkg unregister bytestring-<version>`
*   hide one of them when building: `ghc --make -hide-package bytestring-<version>`
