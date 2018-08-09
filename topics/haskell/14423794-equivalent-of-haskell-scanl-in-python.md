
# Equivalent of Haskell scanl in python

## Question
        
I would like to know if there is a built in function in python for the equivalent Haskell `scanl`, as `reduce` is the equivalent of `foldl`.

Something that does this:

    Prelude> scanl (+) 0 [1 ..10]
    [0,1,3,6,10,15,21,28,36,45,55]
    

The question is not about how to implement it, I already have 2 implementations, shown below (however, if you have a more elegant one please feel free to show it here).

First implementation:

     # Inefficient, uses reduce multiple times
     def scanl(f, base, l):
       ls = [l[0:i] for i in range(1, len(l) + 1)]
       return [base] + [reduce(f, x, base) for x in ls]
    
      print scanl(operator.add, 0, range(1, 11))
    

Gives:

    [0, 1, 3, 6, 10, 15, 21, 28, 36, 45, 55]
    

Second implementation:

     # Efficient, using an accumulator
     def scanl2(f, base, l):
       res = [base]
       acc = base
       for x in l:
         acc = f(acc, x)
         res += [acc]
       return res
    
     print scanl2(operator.add, 0, range(1, 11))
    

Gives:

    [0, 1, 3, 6, 10, 15, 21, 28, 36, 45, 55]
    

Thank you :)

## Answer
        
You can use this, if its more elegant:

    def scanl(f, base, l):
        for x in l:
            base = f(base, x)
            yield base
    

Use it like:

    import operator
    list(scanl(operator.add, 0, range(1,11)))
    

Python 3.x has [`itertools.accumulate(iterable, func= operator.add)`](http://docs.python.org/3/library/itertools.html#itertools.accumulate). It is implemented as below. The implementation might give you ideas:

    def accumulate(iterable, func=operator.add):
        'Return running totals'
        # accumulate([1,2,3,4,5]) --> 1 3 6 10 15
        # accumulate([1,2,3,4,5], operator.mul) --> 1 2 6 24 120
        it = iter(iterable)
        total = next(it)
        yield total
        for element in it:
            total = func(total, element)
            yield total
