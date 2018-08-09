
# Haskell - Accessing a Specific Element in a Tuple

## Question
        
Haskell-newbie reporting in. Question is as follows: In Haskell, we have `fst` and `snd` that return the first and the second elements of a 2-tuple. Why don't we have an easy way of accessing the i-th element from any tuple? Right now I have a 3-tuple, I want to read the 1st element and the only way of accomplishing this task is doing pattern-matching trickery. Why can't this be done easier? Or maybe there is some easy way?

## Answer
        
What prevents the language from having the special construct you want is its design. The designers just didn't put this in, because it would complicate the language definition, which is quite minimalistic. `fst` and `snd` are library functions for the common case of pairs; you can define all the others yourself, or better, define record types for your data so that your data members have appropriate names.

(It may be that GHC has an extension to do this, but I haven't encountered one; check the docs or ask on the mailing list to be sure.)
