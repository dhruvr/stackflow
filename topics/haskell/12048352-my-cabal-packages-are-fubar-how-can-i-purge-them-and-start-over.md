
# My cabal packages are FUBAR; how can I purge them and start over?

## Question
        
I forgot to enable building libs for profiling in my `~/.cabal/config` before installing a bunch of packages on a new machine and now a `--reinstall world` to try to fix the situation has left everything broken (naturally).

I think it's a better use of my time to just purge everything. How do I do that correctly?

## Answer
        
Cabal doesn't keep track of what it's installed, it just uses ghc's library mechanism (or that of some other compiler if you're not using ghc), so you can use `rm -r ~/.ghc` to remove all locally-installed libraries.

If you have multiple ghc's installed, and you only want to remove the libs for a specific ghc, delete the subdirectory corresponding to whichever ghc you want to remove.  
For example, I could remove everything I've installed for ghc-7.6.0 with `rm -r ~/.ghc/x86_64-linux-7.6.0.20120810`  
You can also use this to preserve your ghci_history if you like.

    ll ~/.ghc/
    total 24
    -rw-r--r-- 1 johnl johnl 2300 Aug 21 11:47 ghci_history
    drwxr-xr-x 3 johnl johnl 4096 Jun 17 19:09 x86_64-linux-6.12.3
    drwxr-xr-x 3 johnl johnl 4096 May 17 08:17 x86_64-linux-7.2.1
    drwxr-xr-x 3 johnl johnl 4096 May 16 17:34 x86_64-linux-7.4.1
    drwxr-xr-x 3 johnl johnl 4096 Jun 15 08:21 x86_64-linux-7.4.2
    drwxrwxr-x 3 johnl johnl 4096 Aug 15 12:37 x86_64-linux-7.6.0.20120810
    

Edit: `~/.cabal/world` is a list of installed packages with version constraints as specified by the user. So in most cases, it would include e.g. `mtl -any`. If you've installed packages with specific versions, such as by issuing `cabal install mtl-2.1.1`, it will record that version. You should be able to either delete the world file and start over, or if you look it over and the dependencies are acceptable, you could try just running `cabal install world`. Or you could ignore it and not use cabal's world support (that's what I do).
