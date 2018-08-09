
# Do ghc-compiled binaries require GHC or are they self-contained?

## Question
        
If a friend wants to run my Haskell binaries, does he have to first install Haskell, or can he immediately run the binary by itself?

Is the answer the same on Mac, Windows, and Linux?

## Answer
        
GHC does produce stand-alone binaries that do not require GHC itself to be installed, however they do link against some dynamic libraries, most notably `libgmp`. The remaining libraries are commonly found out of the box on most Linux systems. I believe the situation is similar on Windows.

You can check which dynamic libraries you depend on using `ldd` on Linux. Here's what I get on Ubuntu Natty for a simple Hello World program:

    $ echo 'main = putStrLn "Hello World"' > Hello.hs                                                   
    $ ghc --make Hello.hs                                                                     
    [1 of 1] Compiling Main             ( Hello.hs, Hello.o )
    Linking Hello ...
    $ ldd Hello                                                                                
        linux-vdso.so.1 =>  (0x00007fffe45ff000)
        libgmp.so.3 => /usr/lib/libgmp.so.3 (0x00007f8874cf9000)
        libm.so.6 => /lib/x86_64-linux-gnu/libm.so.6 (0x00007f8874a74000)
        librt.so.1 => /lib/x86_64-linux-gnu/librt.so.1 (0x00007f887486b000)
        libdl.so.2 => /lib/x86_64-linux-gnu/libdl.so.2 (0x00007f8874667000)
        libc.so.6 => /lib/x86_64-linux-gnu/libc.so.6 (0x00007f88742d3000)
        libpthread.so.0 => /lib/x86_64-linux-gnu/libpthread.so.0 (0x00007f88740b4000)
        /lib64/ld-linux-x86-64.so.2 (0x00007f8874f7a000)
