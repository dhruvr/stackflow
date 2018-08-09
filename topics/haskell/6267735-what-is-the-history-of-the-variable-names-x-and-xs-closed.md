
# What is the history of the variable names x and xs? [closed]

## Question
        
I'm trying to pick up a bit of Haskell, and I'm alternating between awe and befuddlement. One of the really alienating things for me, trivial as this may seem, is the pattern matching idiom `(x:xs)`. Where do those variable names come from? They could be anything -- `(kernel:cob)`, `(spam:eggs)` (tipping my hand a bit), or -- most sensibly, to my mind, `(h:t)`, standing for 'head' and 'tail'.

I suppose the `x` prefix is useful for indicating that both items come from the same list, so then `(xh:xt)` or even just `(x:xt)` if you're feeling especially terse. But why _s_? What does it mean? Where did it come from? I feel, at the moment, that knowing would help me cope with my confusion.

Perhaps I am thinking about this in the wrong way; please feel free to tell me so.

## Answer
        
`x` is a common variable name in mathematics. `xs` is the plural form of `x` (get it?). In list pattern matching, `x` is one element, and `xs` is (generally) several.
