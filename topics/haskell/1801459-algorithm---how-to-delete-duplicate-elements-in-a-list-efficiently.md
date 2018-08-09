
# Algorithm - How to delete duplicate elements in a list efficiently?

## Question
        
There is a **list L**. It contains elements of **arbitrary type each**. How to delete all duplicate elements in such list efficiently? **ORDER must be preserved**

Just an algorithm is required, so no import any external library is allowed.

### Related questions

*   [In Python, what is the fastest algorithm for removing duplicates from a list so that all elements are unique _while preserving order_?](https://stackoverflow.com/questions/89178/in-python-what-is-the-fastest-algorithm-for-removing-duplicates-from-a-list-so-t)
    
*   [How do you remove duplicates from a list in Python whilst preserving order?](https://stackoverflow.com/questions/480214/how-do-you-remove-duplicates-from-a-list-in-python-whilst-preserving-order)
    
*   [Removing duplicates from list of lists in Python](https://stackoverflow.com/questions/1143379/removing-duplicates-from-list-of-lists-in-python)
    
*   [How do you remove duplicates from a list in Python?](https://stackoverflow.com/questions/479897/how-do-you-remove-duplicates-from-a-list-in-python)

## Answer
        
Assuming order matters:

*   Create an empty set S and an empty list M.
*   Scan the list L one element at a time.
*   If the element is in the set S, skip it.
*   Otherwise, add it to M and to S.
*   Repeat for all elements in L.
*   Return M.

In Python:

    >>> L = [2, 1, 4, 3, 5, 1, 2, 1, 1, 6, 5]
    >>> S = set()
    >>> M = []
    >>> for e in L:
    ...     if e in S:
    ...         continue
    ...     S.add(e)
    ...     M.append(e)
    ... 
    >>> M
    [2, 1, 4, 3, 5, 6]
    

If order does not matter:

    M = list(set(L))
