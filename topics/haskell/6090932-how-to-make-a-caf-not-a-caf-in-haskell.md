
# How to make a CAF not a CAF in Haskell?

## Question
        
How do I make a Constant Applicative Form into, well, not a Constant Applicative Form, to stop it being retained for the lifetime of the program?

I've tried this approach:

    -- | Dummy parameter to avoid creating a CAF
    twoTrues :: () -> [[[Bool]]]
    twoTrues _ = map (++ (True : repeat False)) . trueBlock <$> [1..]
    

but it doesn't seem to work - the profile shows it as still being retained _and_ still marks it as a CAF.

I've found one relevant Google result on this, [a reply by Simon Peyton-Jones](http://www.mail-archive.com/glasgow-haskell-users@haskell.org/msg12102.html) to Neil Mitchell who asked precisely this question - but that answer refers to a dead link, unfortunately.

## Answer
        
**Generalise.** If you have a constant value, can you generalise this to a function of some variable? The naming of my function in the question, `twoTrues`, immediately suggests that this constant is the third in a sequence `zeroTrues`, `oneTrue`, `twoTrues`, `threeTrues` etc. - and indeed it is. So generalising `twoTrues` into a function `nTrues` _which takes a parameter n_ and deleting `twoTrues`, would eliminate one CAF from the program.

As it happens, in this case, I had only considered the cases `zeroTrues`, `oneTrue` and `twoTrues` for my program because that was all I needed, but my program could naturally be extended to deal with `nTrues` for `n` \> 2 - so generalising to `nTrues` would mean it would make sense to "generalise all the way up" to the users of `zeroTrues`, `oneTrue` etc. That would not always be the case.

Note: there might still be other CAFs to deal with, either in the code, or produced by GHC's "optimisations" (which are not really optimisations in these pathological cases).

This answer may involve more work by the programmer than is strictly necessary, however. It isn't actually necessary to generalise, as Don's answer shows.

On the other hand, in some cases, generalising a constant can make it more clear what you are actually doing, and aid reusability. It can even reveal ways to compute a series of values in a better systematic way, and/or more efficiently.

A note about this particular case (which can be ignored): In this particular case, I would not want to make `nTrues` _itself_ into an infinite list (which would be a CAF again, reintroducing the original problem!) rather than a function. One reason is that while `twoTrues` could be useful in the form of an infinite list, I can't see how it would be useful (in my application, anyway) for `nTrues` to be in the form of an infinite list.
