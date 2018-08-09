
# Counting unique elements in a list

## Question
        
Is there a straight-forward combination of standard higher-order functions to count the unique elements in a list?

For example the result for

    [1, 1, 4, 0, 4, 4]
    

would be something like

    [(1,2), (4,3), (0,1)]

## Answer
        
If order is not important this works:

    map (\xs@(x:_) -> (x, length xs)) . group . sort
    

`group . sort` will give you a list of lists where all elements that are equal to each other are grouped into the same sublist (without sort, only consecutive equal elements would be grouped together). The `map` then turns each sublist into a `(element, lengthOfSublist)`-tuple.

If you want to order the result by first occurrence, you can use `zip` before the sort to add an index to each element, then, after grouping, sort again by that index and then remove the index.
