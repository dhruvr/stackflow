
# Y Combinator in Haskell

## Question
        
Is it possible to write the [Y Combinator](http://www.ece.uc.edu/~franco/C511/html/Scheme/ycomb.html) in Haskell?

It seems like it would have an infinitely recursive type.

     Y :: f -> b -> c
     where f :: (f -> b -> c)
    

or something. Even a simple slightly factored factorial

    factMaker _ 0 = 1
    factMaker fn n = n * ((fn fn) (n -1)
    
    {- to be called as
    (factMaker factMaker) 5
    -}
    

fails with "Occurs check: cannot construct the infinite type: t = t -> t2 -> t1"

(The Y combinator looks like this

    (define Y
        (lambda (X)
          ((lambda (procedure)
             (X (lambda (arg) ((procedure procedure) arg))))
           (lambda (procedure)
             (X (lambda (arg) ((procedure procedure) arg)))))))
    

in scheme) Or, more succinctly as

    (λ (f) ((λ (x) (f (λ (a) ((x x) a))))
            (λ (x) (f (λ (a) ((x x) a))))))
    

For the applicative order And

    (λ (f) ((λ (x) (f (x x)))
            (λ (x) (f (x x)))))
    

Which is just a eta contraction away for the lazy version.

If you prefer short variable names.

## Answer
        
Oh

[this wiki page](http://en.wikipedia.org/wiki/Fixed_point_combinator#Example_of_encoding_via_recursive_types) and [This Stack Overflow answer](https://stackoverflow.com/questions/2003225/rosetta-stone-y-combinator/2006026#2006026) seem to answer my question.  
I will write up more of an explanation later.

Now, I've found something interesting about that Mu type. Consider S = Mu Bool.

    data S = S (S -> Bool)
    

If one treats S as a set and that equals sign as isomorphism, then the equation becomes

    S ⇋ S -> Bool ⇋ Powerset(S)
    

So S is the set of sets that are isomorphic to their powerset! But we know from Cantor's diagonal argument that the cardinality of Powerset(S) is always strictly greater than the cardinality of S, so they are never isomorphic. I think this is why you can now define a fixed point operator, even though you can't without one.
