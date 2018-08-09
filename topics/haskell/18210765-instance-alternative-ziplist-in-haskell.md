
# instance Alternative ZipList in Haskell?

## Question
        
ZipList comes with a Functor and an Applicative instance ([Control.Applicative](http://hackage.haskell.org/packages/archive/base/latest/doc/html/Control-Applicative.html#g:2)) but why not Alternative?

*   Is there no good instance?
*   What about the one proposed below?
    *   Is it flawed?
    *   is it useless?
    *   Are there other reasonable possibilities (like `Bool` can be a monoid in two ways) and therefore neither should be _the_ instance

I searched for "instance Alternative ZipList" (with the quotes to find code first) and only found the library, some tutorials, lecture notes yet no actual instance.

Matt Fenwick said ZipList A will only be a monoid if A is. [See here](https://stackoverflow.com/questions/13080606/confused-by-the-meaning-of-the-alternative-type-class-and-its-relationship-to/13168509#13168509). Lists are monoids though, regardless of the element type.

[This other answer](https://stackoverflow.com/questions/13080606/confused-by-the-meaning-of-the-alternative-type-class-and-its-relationship-to/13174738#13174738) by AndrewC to the same question discusses how an Alternative instance might look like. He says

> There are two sensible choices for Zip `[1,3,4] <|> Zip [10,20,30,40]`:
> 
> 1.  `Zip [1,3,4]` because it's first - consistent with Maybe
> 2.  `Zip [10,20,30,40]` because it's longest - consistent with `Zip []` being discarded

where Zip is basically ZipList. I think the answer should be `Zip [1,3,4,40]`. Let's see an instance:

    instance Aternative Zip where
      empty = Zip []
      Zip xs <|> Zip ys = Zip (go xs ys) where
        go [] ys = ys
        go (x:xs) ys = x : go xs (drop 1 ys)
    

The only `Zip a` we can produce without knowing the type argument `a` is `Zip [] :: Zip a`, so there is little choice for `empty`. If the empty list is the neutral element of the monoid, we might be tempted to use list concatenation. However, `go` is not `(++)` because of the `drop 1`. Every time we use one entry of the first argument list, we drop one of the second. Thus we have a kind of overlay: The left argument list hides the beginning of the right one (or all of it).

    [ 1, 3, 4,40]   [10,20,30,40]   [ 1, 3, 4]   [ 1, 3, 4]
      ^  ^  ^  ^      ^  ^  ^  ^      ^  ^  ^      ^  ^  ^
      |  |  |  |      |  |  |  |      |  |  |      |  |  |
    [ 1, 3, 4] |    [10,20,30,40]   []|  |  |    [ 1, 3, 4]
    [10,20,30,40]   [ 1, 3, 4]      [ 1, 3, 4]   []
    

One intuition behind ziplists is processes: A finite or infinite stream of results. When zipping, we combine streams, which is reflected by the Applicative instance. When the end of the list is reached, the stream doesn't produce further elements. This is where the Alternative instance comes in handy: We can name a replacement, taking over as soon as the default process terminates.

For example we could write `fmap Just foo <|> pure Nothing` to wrap every element of the ziplist `foo` into a `Just` and continue with `Nothing` afterwards. The resulting ziplist is infinite, reverting to a default value after all (real) values have been used up. This could of course be done by hand, by appending an infinite list inside the `Zip` constructor. Yet the above is more elegant and does not assume knowledge of constructors, leading to higher code reusability.

We don't need any assumption on the element type (like being a monoid itself). At the same time the definition is not trivial (as `(<|>) = const` would be). It makes use of the list structure by pattern matching on the first argument.

The definition of `<|>` given above is associative and the empty list really is the empty element. We have

    Zip [] <*> xs = fs <*> Zip [] = Zip []
    (fs <|> gs) <*> xs = fs <*> xs <|> gs <*> xs
    fs <*> (xs <|> ys) = fs <*> xs <|> fs <*> ys
    

so all the laws you could ask for are satisfied (which is not true for list concatenation).

This instance is consistent with the one for Maybe: Choice is biased to the left, yet when the left argument is unable to produce a value, the right argument takes over. The functions

    zipToMaybe :: Zip a -> Maybe a
    zipToMaybe (Zip []) = Nothing
    zipToMaybe (Zip (x:_)) = Just x
    
    maybeToZip :: Maybe a -> Zip a
    maybeToZip Nothing = Zip []
    maybeToZip (Just x) = Zip (repeat x)
    

are morphisms of alternatives (meaning `psi x <|> psi y = psi (x <|> y)` and `psi x <*> psi y = psi (x <*> y)`).

Edit: For the `some`/`many` methods I'd guess

    some (Zip z) = Zip (map repeat z)
    many (Zip z) = Zip (map repeat z ++ repeat [])

## Answer
        
Tags / Indeces
--------------

Interesting. A not completely unrelated thought: ZipLists can be seen as ordinary lists with elements tagged by their (increasing) position index in the list. Zipping application joins two lists by pairing equally-indexed elements.

Imagine lists with elements tagged by (non-decreasing) `Ord` _values_. Zippery _application_ would pair-up equally-tagged elements, throwing away all mismatches ([it has its uses](http://ideone.com/nuoLUE)); zippery _alternative_ could perform order-preserving left-preferring _union_ on tag values (alternative on regular lists is also kind of a union).

This fully agrees with what you propose for indexed lists (aka ZipLists).

So yes, it makes sense.

Streams
-------

One interpretation of a list of values is non-determinacy, which is consistent with the monad instance for lists, but ZipLists can be interpreted as synchronous streams of values which are combined in sequence.

With this stream interpretation it's you don't think in terms of the whole list, so choosing the longest stream is clearly cheating, and the correct interpretation of failing over from the first ZipList to the second in the definition `<|>` would be to do so on the fly as the first finishes, as you say in your instance.

Zipping two lists together doesn't do this simply because of the type signature, but it's the correct interpretation of `<|>`.

Longest Possible List
---------------------

When you zip two lists together, the result is the minimum of the two lengths. This is because that's the longest possible list that meets the type signature without using ⊥. It's a mistake to think of this as picking the shorter of the two lengths - it's the longest possible.

Similarly `<|>` should generate the longest possible list, and it should prefer the left list. Clearly it should take the whole of the left list and take up the right list where the left left off to preserve synchronisation/zippiness.
