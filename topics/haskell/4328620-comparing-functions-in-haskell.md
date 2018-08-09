
# Comparing functions in Haskell

## Question
        
Is there any way to compare two functions in Haskell?

My thought is that the answer is no since functions would not derive the Eq type class. However I'm trying to write a pretty trivial function and it seems like a normal sort of thing to do:

    search :: ((Enum a) => a -> a) -> Card -> [Card]
    search op x list = if (op == succ && rank x == King) || 
                          (op == pred && rank x == Ace)
                       then []
                       else let c = [ n | n <- list, rank n == op (rank x)]
                         in if length c == 1
                         then x : search op (head c) list
                         else []
    

Error message:

    No instance for (Eq (Rank -> Rank))
          arising from a use of `=='
    

Basically it either searches up or down a list of cards looking for a match with either the next or previous ranked card from x, building a list. By taking the 'pred' or 'succ' function as an operator it works both forwards and backwards. However, I need to check that it doesn't go out of bounds on the enum otherwise it throws an exception.

So I'm looking for a way to prevent the exception or solve this problem!

Any other pointers on improving the code would also be appreciated :)

Thanks for all the great tips, this is the solution I have come up with (taken bits from every answer really!):

EDIT: Correct solution below:

     maybeSucc x | x == maxBound = Nothing
                 | otherwise = Just (succ x)
     maybePred x | x == minBound = Nothing  
                 | otherwise = Just (pred x)
    
    -- takes a list of cards which have a rank one op than x
    -- only if there is exactly one is it sequential, otherwise end matching
    search :: (Rank -> Maybe Rank) -> Rank -> [Card] -> [Card]
    search op x list = case filter (\n -> Just (rank n) == op x) list of
                        [y] -> y : search op (rank y) list
                         _ -> []
    

Test:

    *Main> let cards = [Card Ace Heart, Card Two Club, Card Three Spade, Card Five Club, Card Four Diamond]
    
    *Main> search maybeSucc Two cards
    
    [Three of Spades,Four of Diamonds,Five of Clubs]
    
    *Main> search maybePred Three cards
    
    [Two of Clubs,Ace of Hearts]

## Answer
        
1) Your `op` is overly general. You'll only be using it for Card whatever type `rank (undefined :: Card)` is, so just make it `RankThing -> RankThing`. Also, your function type signature is missing a return value type.

2) An example intended use looks like it would be `search succ Ace xs`, but that's rather clumsy, how about two auxillary functions that handle the bounds:

    searchUp King _ = []
    searchUp x ys = search succ x ys
    
    searchDown Ace _ = []
    searchDown x ys = search pred x ys
    

This might read better for your users and avoid the need to check the operation

3) if you really want to check what operation is performed, and know the operation will be one of two possibilities, then you can either name the operation or test it with a known answer test (KAT). For example:

    data Operation = Succ | Pred
    
    search :: Operation -> Card -> [Card] -> []
    search Succ King _ = []
    search Succ x ys = search' succ x ys
    search Pred Ace _ = []
    search Pred x ys = search' pred x ys
    

And the KAT solution (rather lame, imo):

    search op x ys
        | op Five == Four && rank x == Ace = []
        | op Five == Six && rank x == King = []
        | otherwise = ...
