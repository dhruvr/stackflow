
# Calling Haskell functions from Python

## Question
        
I want to use some Haskell libraries (e.g. Darcs, Pandoc) from Python, but it seems there’s no direct foreign function interface to Haskell in Python. Is there any way to do that?

## Answer
        
Provided you can get your Python code to call C, you can call Haskell functions that have been exported via the [FFI](http://www.haskell.org/haskellwiki/FFI_Introduction)

Another approach would be to write a standard IPC interface, in the case of darcs and pandoc just calling them as vanilla executables and parsing their output might be the way to go.

As to automating the generation of boring, repetitive, FFI and marshalling code on the Haskell side, I'd recommend [c2hs](http://hackage.haskell.org/package/c2hs), which allows you to auto-generate a lot based on an existing C interface. There's probably similar things for python.

SWIG, alas, has, to the best of my knowledge, never been implemented for Haskell, presumably because it caters to less strictly-typed languages.
