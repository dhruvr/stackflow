
# Why is GHC so large/big?

## Question
        
Is there a simple answer: Why is GHC so big?

*   OCaml: 2MB
*   Python: 15MB
*   SBCL: 9MB
*   OpenJRE - 26MB
*   **GHC: 113MB**

Not interested in evangelism of "Why I shouldn't care about the size if Haskell is the right tool"; this is a technical question.

## Answer
        
It's a bit silly really. Every library that comes with GHC is provided in no less than **4 flavours**:

*   static
*   dynamic
*   profiled
*   GHCi

The GHCi version is just the static version linked together in a single `.o` file. The other three versions all have their own set of interface files (`.hi` files) too. The profiled versions seem to be about twice the size of the unprofiled versions (which is a bit suspicious, I should look into why that is).

Remember that **GHC itself is a library**, so you're getting 4 copies of GHC. Not only that, but the GHC binary itself is statically linked, so that's 5 copies of GHC.

We recently made it so that GHCi could use the static `.a` files. That will allow us to get rid of one of these flavours. Longer term, we should dynamically link GHC, but that's a bigger change because that would entail making dynamic linking the default - unlike in C, with GHC you have to decide up front whether you're going to link dynamically or not. And we need more changes (e.g. to Cabal and the package system, amongst other things) before this is really practical.
