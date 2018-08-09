
# Statically link GMP to an Haskell application using GHC (+ LLVM)

## Question
        
1.  How can I drop dynamic dependency on `libgmp` and go from this:
    
        linux-vdso.so.1 =>  (0x00007fffdccb1000)
        libgmp.so.10 => /usr/lib/x86_64-linux-gnu/libgmp.so.10 (0x00007fb01afc1000)
        libm.so.6 => /lib/x86_64-linux-gnu/libm.so.6 (0x00007fb01acc7000)
        librt.so.1 => /lib/x86_64-linux-gnu/librt.so.1 (0x00007fb01aabe000)
        libdl.so.2 => /lib/x86_64-linux-gnu/libdl.so.2 (0x00007fb01a8ba000)
        libpthread.so.0 => /lib/x86_64-linux-gnu/libpthread.so.0 (0x00007fb01a69d000)
        libc.so.6 => /lib/x86_64-linux-gnu/libc.so.6 (0x00007fb01a2df000)
        /lib64/ld-linux-x86-64.so.2 (0x00007fb01b249000)
        
    
    to this (currently desired):
    
        linux-vdso.so.1 =>  (0x00007fffdccb1000)
        libm.so.6 => /lib/x86_64-linux-gnu/libm.so.6 (0x00007fb01acc7000)
        librt.so.1 => /lib/x86_64-linux-gnu/librt.so.1 (0x00007fb01aabe000)
        libdl.so.2 => /lib/x86_64-linux-gnu/libdl.so.2 (0x00007fb01a8ba000)
        libpthread.so.0 => /lib/x86_64-linux-gnu/libpthread.so.0 (0x00007fb01a69d000)
        libc.so.6 => /lib/x86_64-linux-gnu/libc.so.6 (0x00007fb01a2df000)
        /lib64/ld-linux-x86-64.so.2 (0x00007fb01b249000)
        
    
    in a clean and portable way that just works on all GNU/Linux distributions (and not messing up with BSDs (including OS X))?
    
2.  Do you see any other dependencies that may cause problems in the currently desired list as given above when distributing a single Haskell binary targeting multiple GNU/Linux distributions?
    

* * *

**Notes:**

*   my app is GPLv3 so no license violation issues arise regarding GMP
*   Specifying a path to `libgmp.a` does not work ( [How to selectively link certain system libraries statically into Haskell program binary?](https://stackoverflow.com/questions/7832112/how-to-selectively-link-certain-system-libraries-statically-into-haskell-program) ), `libgmp` is still listed in the `ldd` output.

## Answer
        
If you pass `-optl-static -optl-pthread` to GHC, it'll statically link all the runtime library dependencies, including GMP. Setting `ld-options: -static -pthread` in your Cabal file should accomplish the same thing.

That means you'll statically link in glibc too, but that probably won't be a problem, although it might increase binary size quite a bit. Using an alternative libc like [musl](http://www.etalabs.net/musl/) or [uClibc](http://www.uclibc.org/) should help counteract that, if it's a problem for you.
