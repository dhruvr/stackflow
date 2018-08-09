
# Calling Haskell from C#

## Question
        
I just spent the last week or so figuring out how to execute C++ code from C# as part of my day job. It took us forever to figure it out, but the final solution is fairly simple.

Now I'm curious... How hard would it be to call Haskell from C#? (Note carefully: That's call Haskell _from_ C#, not the other way around. So the main executable is C#.)

If it's really hard, I won't bother. But if it's reasonably easy, I might have to have a play with it...

Basically, we wrote some C++ code. On Windows it gets compiled into a DLL, on Linux it gets compiled into a shared object (`*.so`). Then on the C# side you do a `DllImport` and write some manual memory management code if you're trying to pass anything nontrivial across. (E.g., arrays, strings, etc.)

I know GHC is supposed to support building shared libraries on both platforms, but I'm not sure of the technical details. What's the syntax for exporting stuff, and does the caller have to do anything special to initialise the DLL first?

To be concrete: Suppose there exists a function `foobar :: FilePath -> IO Int32`. Can somebody throw together a small sketch showing:

*   What Haskell declarations I need to write to expose this to the outside world.
*   How do I tell GHC to build a single self-contained DLL / SO file.
*   Anything special the caller needs to do, beyond the usual process of binding `foobar` itself.

I'm not too worried about the actual syntax for the C# side; I think I've more or less puzzled that out.

P.S. I did briefly look at `hs-dotnet`, but this appears to be Windows-specific. (I.e., won't work with Mono, so won't work on Linux.)

## Answer
        
As far as both languages are concerned, you can basically pretend you're trying to interface with C code.

This is a complex topic, so rather than try to explain all of it, I will focus on making a simple example that you can build on using the resources linked below.

1.  First, you need to write wrappers for your Haskell functions that use types from the `Foreign.C.*` modules instead of the usual haskell types. `CInt` instead of `Int`, `CString` instead of `String`, etc. This is the most complicated step, especially when you have to deal with user-defined types.
    
    You also have to write `foreign export` declarations for those functions using the `ForeignFunctionInterface` extension.
    
        {-# LANGUAGE ForeignFunctionInterface #-}
        module Foo where
        
        import Foreign.C.String
        import Foreign.C.Types
        
        foreign export ccall
          foo :: CString -> IO CInt
        
        foo :: CString -> IO CInt
        foo c_str = do
          str    <- peekCString c_str
          result <- hs_foo str 
         return $ fromIntegral result
        
        hs_foo :: String -> IO Int
        hs_foo str = do
          putStrLn $ "Hello, " ++ str
          return (length str + 42)
        
    
2.  Then, when compiling, you tell GHC to make a shared library:
    
        $ ghc -O2 --make -no-hs-main -optl '-shared' -o Foo.so Foo.hs
        
    
3.  From the C# side, in addition to importing the function you want to call, you also have to import `hs_init()` and call it to initialize the runtime system before you can call any Haskell functions. You should also call `hs_exit()` when you're done.
    
        using System;
        using System.Runtime.InteropServices;
        
        namespace Foo {
            class MainClass {
                [DllImport("Foo.so", CallingConvention = CallingConvention.Cdecl)]
                private static extern void hs_init(IntPtr argc, IntPtr argv);
        
                [DllImport("Foo.so", CallingConvention = CallingConvention.Cdecl)]
                private static extern void hs_exit();
        
                [DllImport("Foo.so", CallingConvention = CallingConvention.Cdecl)]
                private static extern int foo(string str);
        
                public static void Main(string[] args) {
                    Console.WriteLine("Initializing runtime...");
                    hs_init(IntPtr.Zero, IntPtr.Zero);
        
                    try {
                        Console.WriteLine("Calling to Haskell...");
                        int result = foo("C#");
                        Console.WriteLine("Got result: {0}", result);
                    } finally {
                        Console.WriteLine("Exiting runtime...");
                        hs_exit();
                    }
                }
            }
        }
        
    
4.  Now we compile and run:
    
        $ mcs -unsafe Foo.cs
        $ LD_LIBRARY_PATH=. mono Foo.exe
        Initializing runtime...
        Calling to Haskell...
        Hello, C#
        Got result: 44
        Exiting runtime...
        
    
    It works!
    

**Resources:**

*   [GHC User's Guide](http://www.haskell.org/ghc/docs/latest/html/users_guide/ffi.html)
*   [HaskellWiki](http://www.haskell.org/haskellwiki/Foreign_Function_Interface)
