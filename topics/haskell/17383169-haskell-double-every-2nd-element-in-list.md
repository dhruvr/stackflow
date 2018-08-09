
# Haskell: Double every 2nd element in list

## Question
        
I just started using Haskell and wanted to write a function that, given a list, returns a list in which every 2nd element has been doubled.

So far I've come up with this:

    double_2nd :: [Int] -> [Int]
    double_2nd [] = []
    double_2nd (x:xs) = x : (2 * head xs) : double_2nd (tail xs)
    

Which works but I was wondering how you guys would write that function. Is there a more common/better way or does this look about right?

## Answer
        
That's not bad, modulo the fixes suggested. Once you get more familiar with the base library you'll likely avoid explicit recursion in favor of some higher level functions, for example, you could create a list of functions where every other one is `*2` and apply (zip) that list of functions to your list of numbers:

    double = zipWith ($) (cycle [id,(*2)])
