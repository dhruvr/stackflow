
# Haskell Tuple Size Limit

## Question
        
Why I can't construct large tuples in Haskell? Why there's a tuple size limit?

    Prelude> (1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)
    
    <interactive>:1:0:
        No instance for (Show
                           (t,
                            t1,
                            t2,
                            ...
                            t23))
          arising from a use of `print' at <interactive>:1:0-48
        Possible fix:
          add an instance declaration for
          (Show
             (t,
              t1,
              t2,
              ...
              t23))
        In a stmt of a 'do' expression: print it

## Answer
        
Tuples can be of arbitrary length*, but Show, as well as Eq, Ord, Read, Bounded, etc are only instantiated up to 15-tuple. From the [Haskell 98 report §6.1.4](http://www.haskell.org/onlinereport/basic.html#basic-tuples):

> **There is no upper bound on the size of a tuple**, but some Haskell implementations may restrict the size of tuples, and limit the instances associated with larger tuples. **However, every Haskell implementation must support tuples up to size 15, together with the instances for Eq, Ord, Bounded, Read, and Show.** The Prelude and libraries define tuple functions such as zip for tuples up to a size of 7.

As others have said, if you need a 24-tuple, you should use a better data structure.

* * *

Edit: * as of GHC 6.12.2, the maximum size of a tuple is 62:

    Prelude> :t (1,2,3,4,5,6,7,8,1,2,3,4,5,6,7,8,1,2,3,4,5,6,7,8,1,2,3,4,5,6,7,8,1,2,3,4,5,6,7,8,1,2,3,4,5,6,7,8,1,2,3,4,5,6,7,8,1,2,3,4,5,6,7,8)
    
    <interactive>:1:0:
        A 64-tuple is too large for GHC
          (max size is 62)
          Workaround: use nested tuples or define a data type
