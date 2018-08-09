
# Updating record when referenced by multiple data structures

## Question
        
Suppose I have a record, e.g. `Person`, and I want to be able to look this person up through multiple data structures. Maybe there's an index by name, another index by the person's zip code, and another index by the person's current latitude and longitude. And maybe many more data structures. All of which exist because I need to efficiently look up a person or persons with different criteria.

If I just need to _read_ a person's attributes, [this is no problem](https://stackoverflow.com/questions/16575361/multiple-lookup-structures-for-same-data-memory-duplication). But now suppose I need to look up a person using one of these data structures and then _update_ the person's data.

In an OOP language, each data structure would point to the same person in memory. So when you update one, you're implicitly updating the referents of the other data structures as well. This is pretty much the definition of side effects and impurity. I know it's totally counter to the Haskell paradigm, and I'm not expecting Haskell to work this way.

So, what is the Haskell-ish way to do it? To be clear, the problem is this: I look up a person by one data structure, and I pass that person (and maybe some other arbitrary data) into a function of type `ArbitraryData -> Person -> Person`. How do I propagate this change across all the various lookup structures?

As a relative newcomer to Haskell, my first instinct is to reconstruct every lookup structure with the newly updated person, every time I update a person. But that seems like a lot of ceremony, with plenty of opportunity for me to screw up in a way GHC can't detect, and not at all elegant. Haskell is known for its elegance, and I can't imagine it lacks an elegant solution to such a common and basic problem. So I think I'm missing something.

For reference, this question expands on some of the issues I was discussing in the following questions:

[Multiple lookup structures for same data: Memory duplication?](https://stackoverflow.com/questions/16575361/multiple-lookup-structures-for-same-data-memory-duplication)

[Identity of simulation objects in Haskell](https://stackoverflow.com/questions/19229586/identity-of-simulation-objects-in-haskell)

**Edit**

One solution that just crossed my mind: Don't maintain a copy of each lookup structure in your main state. Just keep one single list of all persons in existence, and that's the only thing we need to update when we update a person. Every time you need to lookup by, say, zip code, pass the list of all persons into a function that generates the efficient by-zip-code data structure. Then perform the lookup on the result.

I don't know if this would be efficient. If it results in the CPU actually recomputing the lookup structure on each use, it's unacceptable. But I know Haskell sometimes can avoid reevaluating identical expressions. Unfortunately, I still haven't figured out _when_ this is the case. So I don't know if this approach is viable.

So in other words: Can I write my functions _as if_ they're computing the lookup each time, when in fact GHC will optimize it away for cases where the underlying data hasn't changed? Because that would be a _very_ elegant solution to the problem I've identified above.

## Answer
        
Since I answered this, a few people in #haskell on Freenode recommended alternative, premade solutions:

*   mm\_freak\_ recommended [`Data.IxSet`](http://hackage.haskell.org/package/ixset/docs/Data-IxSet.html).
*   donri recommended [`Data.Store`](http://hackage.haskell.org/package/data-store-0.3.0.4/docs/Data-Store.html), which was said to provide lenses.

* * *

You can make a data structure that contains your lookup tables, as well as a [`Vector`](http://hackage.haskell.org/package/vector) of actual `Person`s. The lookup tables will give you an `Int` or a list of `Int`s (rather than a `Person` or a list of `Person`s) which is the index into the `Vector Person`. For example:

    data PersonStuff = PersonStuff {
                                     persons              :: Vector Person,
                                     firstNameLookupTable :: LookupTable Name,
                                     ...
                                   }
    
    data LookupTable a = LookupTable {
                                       table  :: Map a Int,
                                       update :: Person -> Person -> Map a Int -> Map a Int
                                     }
    

The `update` function is given the old `Person`, the updated `Person`, and will update the table only if the relevant details have changed. When a `Person` is modified through the convenient `PersonStuff` functions you'll write, those functions will update all the lookup tables for you, returning a new `PersonStuff` with all associated data. This makes for a pure data structure with quick lookups.

You can make functions like `updatePeopleWithFirstName :: Name -> (Person -> Person) -> PersonStuff -> PersonStuff` that will get all the people with a first name, apply a `Person -> Person` to each of them, modify their entries in the `Vector`, and use the `update` functions to update all of the lookupTables.
