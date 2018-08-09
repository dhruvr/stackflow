
# Is there a way to see the list of functions in a module, in GHCI?

## Question
        
I find it handy in Python or Common Lisp that you can list a library's contents at runtime. Does Haskell have the same thing, in particular from a GHCI prompt?

## Answer
        
GHCi has a `:browse` command to list the contents of modules:

    Prelude> :browse Data.List
    (\\) :: (Eq a) => [a] -> [a] -> [a]
    delete :: (Eq a) => a -> [a] -> [a]
    deleteBy :: (a -> a -> Bool) -> a -> [a] -> [a]
    deleteFirstsBy :: (a -> a -> Bool) -> [a] -> [a] -> [a]
    elemIndex :: (Eq a) => a -> [a] -> Maybe Int
    ...
    Prelude> :help                    
    ...
       :browse[!] [[*]<mod>]       display the names defined by module <mod>
                                   (!: more details; *: all top-level names)
    ...
