
# Haskell: Lists, Arrays, Vectors, Sequences

## Question
        
I'm learning Haskell and read a couple of articles regarding performance differences of Haskell lists and (insert your language)'s arrays.

Being a learner I obviously just use lists without even thinking about performance difference. I recently started investigating and found numerous data structure libraries available in Haskell.

Can someone please explain the difference between Lists, Arrays, Vectors, Sequences without going very deep in computer science theory of data structures?

Also, are there some common patterns where you would use one data structure instead of another?

Are there any other forms of data structures that I am missing and might be useful?

## Answer
        
Lists Rock
----------

By far the most friendly data structure for sequential data in Haskell is the List

     data [a] = a:[a] | []
    

Lists give you ϴ(1) cons and pattern matching. The standard library, and for that matter the prelude, is full of useful list functions that should litter your code (`foldr`,`map`,`filter`). Lists are _persistant_ , aka purely functional, which is very nice. Haskell lists aren't really "lists" because they are coinductive (other languages call these streams) so things like

    ones :: [Integer]
    ones = 1:ones
    
    twos = map (+1) ones
    
    tenTwos = take 10 twos
    

work wonderfully. Infinite data structures rock.

Lists in Haskell provide an interface much like iterators in imperative languages (because of laziness). So, it makes sense that they are widely used.

On the other hand
=================

The first problem with lists is that to index into them `(!!)` takes ϴ(k) time, which is annoying. Also, appends can be slow `++`, but Haskell's lazy evaluation model means that these can be treated as fully amortized, if they happen at all.

The second problem with lists is that they have poor data locality. Real processors incur high constants when objects in memory are not laid out next to each other. So, in C++ `std::vector` has faster "snoc" (putting objects at the end) than any pure linked list data structure I know of, although this is not a persistant data structure so less friendly than Haskell's lists.

The third problem with lists is that they have poor space efficiency. Bunches of extra pointers push up your storage (by a constant factor).

Sequences Are Functional
------------------------

`Data.Sequence` is internally based on [finger trees](http://en.wikipedia.org/wiki/Finger_tree) (I know, you don't want to know this) which means that they have some nice properties

1.  Purely functional. `Data.Sequence` is a fully persistant data structure.
2.  Darn fast access to the beginning and end of the tree. ϴ(1) (amortized) to get the first or last element, or to append trees. At the thing lists are fastest at, `Data.Sequence` is at most a constant slower.
3.  ϴ(log n) access to the middle of the sequence. This includes inserting values to make new sequences
4.  High quality API

On the other hand, `Data.Sequence` doesn't do much for the data locality problem, and only works for finite collections (it is less lazy than lists)

Arrays are not for the faint of heart
-------------------------------------

Arrays are one of the most important data structures in CS, but they dont fit very well with the lazy pure functional world. Arrays provide ϴ(1) access to the middle of the collection and exceptionally good data locality/constant factors. But, since they dont fit very well into Haskell, they are a pain to use. There are actually a multitude of different array types in the current standard library. These include fully persistant arrays, mutable arrays for the IO monad, mutable arrays for the ST monad, and un-boxed versions of the above. For more check out [the haskell wiki](http://www.haskell.org/haskellwiki/Arrays)

Vector is a "better" Array
--------------------------

The `Data.Vector` package provides all of the array goodness, in a higher level and cleaner API. Unless you really know what you are doing, you should use these if you need array like performance. Of-course, some caveats still apply--mutable array like data structures just dont play nice in pure lazy languages. Still, sometimes you want that O(1) performance, and `Data.Vector` gives it to you in a useable package.

You have other options
----------------------

If you just want lists with the ability to efficiently insert at the end, you can use a [difference list](http://www.haskell.org/haskellwiki/Difference_list). The best example of lists screwing up performance tends to come from `[Char]` which the prelude has aliased as `String`. `Char` lists are convient, but tend to run on the order of 20 times slower than C strings, so feel free to use `Data.Text` or the very fast `Data.ByteString`. I'm sure there are other sequence oriented libraries I'm not thinking of right now.

Conclusion
----------

90+% of the time I need a sequential collection in Haskell lists are the right data structure. Lists are like iterators, functions that consume lists can easily be used with any of these other data structures using the `toList` functions they come with. In a better world the prelude would be fully parametric as to what container type it uses, but currently `[]` litters the standard library. So, using lists (almost) every where is definitely okay.  
You can get fully parametric versions of most of the list functions (and are noble to use them)

    Prelude.map                --->  Prelude.fmap (works for every Functor)
    Prelude.foldr/foldl/etc    --->  Data.Foldable.foldr/foldl/etc
    Prelude.sequence           --->  Data.Traversable.sequence
    etc
    

In fact, `Data.Traversable` defines an API that is more or less universal across any thing "list like".

Still, although you can be good and write only fully parametric code, most of us are not and use list all over the place. If you are learning, I strongly suggest you do too.

* * *

EDIT: Based on comments I realize I never explained when to use `Data.Vector` vs `Data.Sequence`. Arrays and Vectors provide extremely fast indexing and slicing operations, but are fundamentally transient (imperative) data structures. Pure functional data structures like `Data.Sequence` and `[]` let efficiently produce _new_ values from old values as if you had modified the old values.

      newList oldList = 7 : drop 5 oldList
    

doesn't modify old list, and it doesn't have to copy it. So even if `oldList` is incredibly long, this "modification" will be very fast. Similarly

      newSequence newValue oldSequence = Sequence.update 3000 newValue oldSequence 
    

will produce a new sequence with a `newValue` for in the place of its 3000 element. Again, it doesn't destroy the old sequence, it just creates a new one. But, it does this very efficiently, taking O(log(min(k,k-n)) where n is the length of the sequence, and k is the index you modify.

You cant easily do this with `Vectors` and `Arrays`. They can be _modified_ but that is real imperative modification, and so cant be done in regular Haskell code. That means operations in the `Vector` package that make modifications like `snoc` and `cons` have to copy the entire vector so take `O(n)` time. The only exception to this is that you can use the mutable version (`Vector.Mutable`) inside the `ST` monad (or `IO`) and do all your modifications just like you would in an imperative language. When you are done, you "freeze" your vector to turn in into the immutable structure you want to use with pure code.

My feeling is that you should default to using `Data.Sequence` if a list is not appropriate. Use `Data.Vector` only if your usage pattern doesn't involve making many modifications, or if you need extremely high performance within the ST/IO monads.

If all this talk of the `ST` monad is leaving you confused: all the more reason to stick to pure fast and beautiful `Data.Sequence`.
