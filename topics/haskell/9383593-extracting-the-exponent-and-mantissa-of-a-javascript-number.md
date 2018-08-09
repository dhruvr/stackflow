
# Extracting the exponent and mantissa of a Javascript Number

## Question
        
Is there a reasonably fast way to extract the exponent and mantissa from a Number in Javascript?

AFAIK there's no way to get at the bits behind a Number in Javascript, which makes it seem to me that I'm looking at a factorization problem: finding `m` and `n` such that `2^n * m = k` for a given `k`. Since integer factorization is in NP, I can only assume that this would be a fairly hard problem.

I'm implementing a GHC plugin for generating Javascript and need to implement the `decodeFloat_Int#` and `decodeDouble_2Int#` [primitive operations](http://www.haskell.org/ghc/docs/7.2.2/html/libraries/ghc-prim-0.2.0.0/GHC-Prim.html); I guess I could just rewrite the parts of the base library that uses the operation to do wahtever they're doing in some other way (which shouldn't be too hard since all numeric types have Number as their representation anyway,) but it'd be nice if I didn't have to.

Is there any way to do this in an even remotely performant way, by some dark Javascript voodoo, clever mathematics or some other means, or should I just buckle down and have at the base library?

**EDIT** Based on ruakh's and Louis Wasserman's excellent answers, I came up with the following implementation, which seems to work well enough:

    function getNumberParts(x) {
        if(isNaN(x)) {
            return {mantissa: -6755399441055744, exponent: 972};
        }
        var sig = x > 0 ? 1 : -1;
        if(!isFinite(x)) {
            return {mantissa: sig * 4503599627370496, exponent: 972};
        }
        x = Math.abs(x);
        var exp = Math.floor(Math.log(x)*Math.LOG2E)-52;
        var man = x/Math.pow(2, exp);
        return {mantissa: sig*man, exponent: exp};
    }

## Answer
        
ECMAScript doesn't define any straightforward way to do this; but for what it's worth, this isn't a "factorization problem" in the same sense as prime factorization.

What you want can theoretically be done very quickly by first handling the sign, then using a binary-tree approach (or logarithm) to find the exponent, and lastly dividing by the relevant power of two to get the mantissa; but unfortunately, it can be somewhat tricky to implement this in practice (what with special cases such as denormalized numbers). I recommend you read through section 8.5 of the ECMAScript specification to get a sense of what cases you'll have to handle.
