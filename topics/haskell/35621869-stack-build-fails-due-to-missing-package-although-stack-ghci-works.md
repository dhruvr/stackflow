
# Stack build fails due to missing package although stack ghci works

## Question
        
I am trying to build a simple program in Haskell using stack. I created a new project using `stack new` and did a `stack setup` after that. The template builds fine.

I want to experiment with binary file parsing, so I imported `Data.ByteString`. My `build-depends` in the cabal file look like this:

    build-depends:     base >= 4.7 && < 5
                     , bytestring >= 0.10.6
                     , binary >= 0.7.5
    

`stack ghci` now just works, but `stack build` is still not happy. Can someone tell me what I did wrong here?

Here is the complete error message:

    test-0.1.0.0: build
    Preprocessing library test-0.1.0.0...
    In-place registering test-0.1.0.0...
    Preprocessing executable 'test-exe' for test-0.1.0.0...
    
    haskell/test/app/Main.hs:4:18:
        Could not find module ‘Data.ByteString’
        It is a member of the hidden package ‘bytestring-0.10.6.0@bytes_6VWy06pWzJq9evDvK2d4w6’.
        Perhaps you need to add ‘bytestring’ to the build-depends in your .cabal file.
        Use -v to see a list of the files searched for.
    
    haskell/test/app/Main.hs:5:8:
        Could not find module ‘Data.Binary.Get’
        It is a member of the hidden package ‘binary-0.7.5.0@binar_3uXFWMoAGBg0xKP9MHKRwi’.
        Perhaps you need to add ‘binary’ to the build-depends in your .cabal file.
        Use -v to see a list of the files searched for.
    
    --  While building package test-0.1.0.0 using:
          .stack/setup-exe-cache/x86_64-osx/setup-Simple-Cabal-1.22.5.0-ghc-7.10.3 --builddir=.stack-work/dist/x86_64-osx/Cabal-1.22.5.0 build lib:test exe:test-exe --ghc-options " -ddump-hi -ddump-to-file"
        Process exited with code: ExitFailure 1
    

and this is my app/Main.hs file:

    module Main where
    
    import Lib
    import qualified Data.ByteString as B
    import Data.Binary.Get
    import Data.Word
    
    main :: IO ()
    main =  do
        putStrLn "f"
    

Thank you very much for your help.

## Answer
        
This is likely because you added `bytestring` to the `build-depends` of the library, not the executable. One option to avoid needing to repeat these dependencies for the different stanzas is to use [hpack](https://github.com/sol/hpack) as the package description format.
