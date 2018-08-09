
# What Haskell representation is recommended for 2D, unboxed pixel arrays with millions of pixels?

## Question
        
I want to tackle some image-processing problems in Haskell. I'm working with both bitonal (bitmap) and color images with millions of pixels. I have a number of questions:

1.  On what basis should I choose between `Vector.Unboxed` and `UArray`? They are both unboxed arrays, but the `Vector` abstraction seems heavily advertised, particular around loop fusion. Is `Vector` always better? If not, **when should I use which representation?**
    
2.  For color images I will wish to store triples of 16-bit integers or triples of single-precision floating-point numbers. For this purpose, is either `Vector` or `UArray` easier to use? More performant?
    
3.  For bitonal images I will need to store only 1 bit per pixel. Is there a predefined datatype that can help me here by packing multiple pixels into a word, or am I on my own?
    
4.  Finally, my arrays are two-dimensional. I suppose I could deal with the extra indirection imposed by a representation as "array of arrays" (or vector of vectors), but I'd prefer an abstraction that has index-mapping support. Can anyone recommend anything from a standard library or from Hackage?
    

I am a functional programmer and have no need for mutation :-)

## Answer
        
For multi-dimensional arrays, the current best option in Haskell, in my view, is **_[repa](http://hackage.haskell.org/package/repa)_**.

> Repa provides high performance, regular, multi-dimensional, shape polymorphic parallel arrays. All numeric data is stored unboxed. Functions written with the Repa combinators are automatically parallel provided you supply +RTS -Nwhatever on the command line when running the program.

Recently, it has been used for some image processing problems:

*   [Real time edge detection](http://disciple-devel.blogspot.com/2011/03/real-time-edge-detection-in-haskell.html)
*   [Efﬁcient Parallel Stencil Convolution in Haskell](http://www.cse.unsw.edu.au/~benl/papers/stencil/stencil-icfp2011-sub.pdf)

I've started writing **[a tutorial on the use of repa](http://haskell.org/haskellwiki/Numeric_Haskell:_A_Repa_Tutorial)**, which is a good place to start if you already know Haskell arrays, or the vector library. The key stepping stone is the use of shape types instead of simple index types, to address multidimensional indices (and even stencils).

The [repa-io](http://hackage.haskell.org/package/repa-io) package includes support for reading and writing .bmp image files, though support for more formats is needed.

Addressing your specific questions, here is a graphic, with discussion:

* * *

![All three of UArray, Vector, and Repa support unboxing. Vector and Repa have a rich, flexible API, but UArray does not. UArray and Repa have multi-dimensional indexing, but Vector does not. They all have support for bit-packing, although Vector and Repa have some caveats in that regard. Vector and Repa interoperate with C data and code, but UArray does not. Only Repa supports stencils.](https://i.stack.imgur.com/dDAXD.png)

* * *

_On what basis should I choose between Vector.Unboxed and UArray?_

They have approximately the same underlying representation, however, the primary difference is the breadth of the API for working with vectors: they have almost all the operations you'd normally associate with lists (with a fusion-driven optimization framework), while `UArray` have almost no API.

_For color images I will wish to store triples of 16-bit integers or triples of single-precision floating-point numbers._

`UArray` has better support for multi-dimensional data, as it can use arbitrary data types for indexing. While this is possible in `Vector` (by writing an instance of `UA` for your element type), it isn't the primary goal of `Vector` \-\- instead, this is where `Repa` steps in, making it very easy to use custom data types stored in an efficient manner, thanks to the _shape_ indexing.

In `Repa`, your triple of shorts would have the type:

    Array DIM3 Word16
    

That is, a 3D array of Word16s.

_For bitonal images I will need to store only 1 bit per pixel._

UArrays pack Bools as bits, Vector uses the instance for Bool which does do bit packing, instead using a representation based on `Word8`. Howver, it is easy to write a bit-packing implementation for vectors -- [here is one](http://hpaste.org/46709/bit_packing_bools), from the (obsolete) uvector library. Under the hood, `Repa` uses `Vectors`, so I think it inherits that libraries representation choices.

_Is there a predefined datatype that can help me here by packing multiple pixels into a word_

You can use the existing instances for any of the libraries, for different word types, but you may need to write a few helpers using Data.Bits to roll and unroll packed data.

_Finally, my arrays are two-dimensional_

UArray and Repa support efficient multi-dimensional arrays. Repa also has a rich interface for doing so. Vector on its own does not.

* * *

_Notable mentions:_

*   [hmatrix](http://hackage.haskell.org/package/hmatrix), a custom array type with extensive bindings to linear algebra packages. Should be bound to use the `vector` or `repa` types.
*   [ix-shapeable](http://hackage.haskell.org/package/ix-shapable), getting more flexible indexing from regular arrays
*   [chalkboard](http://hackage.haskell.org/package/chalkboard), Andy Gill's library for manipulating 2D images
*   [codec-image-devil](http://hackage.haskell.org/package/Codec-Image-DevIL), read and write various image formats to UArray
