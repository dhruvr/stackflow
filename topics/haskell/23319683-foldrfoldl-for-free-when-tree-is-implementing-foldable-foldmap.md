
# Foldr/Foldl for free when Tree is implementing Foldable foldmap?

## Question
        
I am beginner at a Haskell and learning from "Learn You a Haskell" There's something I don't understand about the Tree implementation of Foldable.

    instance F.Foldable Tree where  
        foldMap f Empty = mempty  
        foldMap f (Node x l r) = F.foldMap f l `mappend`  
                                 f x           `mappend`  
                                 F.foldMap f r  
    

Quote from: LYOH: " So if we just implement foldMap for some type, we get **foldr and foldl on that type for free**! "

Can someone explain that? I don't understand why and how I get foldr and foldl for free now..

## Answer
        
foldr can always be defined as:

    foldr f z t = appEndo (foldMap (Endo . f) t) z
    

where appEndo and Endo are just newtype unwrappers/wrappers. In fact, this code got pulled straight from the Foldable typeclass. So, by defining foldMap, you automatically get foldr.
