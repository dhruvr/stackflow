
# Why shouldn&apos;t I mix tabs and spaces?

## Question
      
I often read that I shouldn't mix tabs and spaces in Haskell, or that I shouldn't use tabs at all. Why?
## Answer
      
The problem is twofold. First of all, Haskell is indentation sensitive, e.g. the following code isn't valid:

    example = (a, b)
      where
        a = "Hello"
         b = "World"
    

Both bindings need to be indented with the same number of spaces/tabs (see [off-side rule](https://en.wikipedia.org/wiki/Off-side_rule)). While it's obvious in this case, it's rather hidden in the following one, where I denote a space by `·` and a tab by `»`:

    example = (a, b)
    ··where
    ····a = "Hello"
    »   b = "World"
    

This will look like valid Haskell code if the editor will show tabs aligned to multiples by four. But it isn't. Haskell tabs are aligned by multiples of eight, so the code will be interpreted like this:

    example = (a, b)
    ··where
    ····a = "Hello"
    »       b = "World"
    

Second, if you use only tabs, you can end up with a layout that doesn't look right. For example, the following code looks correct if a tab gets displayed with six or more spaces (eight in this case):

    example = (a, b)
    »       where»  a = "Hello"
    »       »       b = "World"
    

But in another editor that uses 4 spaces it won't look right anymore:

    example = (a, b)
    »   where»  a = "Hello"
    »   »   b = "World"
    

It's still correct, though. However, someone who's used to spaces might reindent `b`' binding with spaces and end up with a parser error.

If you enforce a code convention throughout your code that makes sure that you only use tabs at the beginning of a line and use a newline after `where`, `let` or `do` you can avoid some of the problems (see [11](http://dmwit.com/tabs/)). However, current releases of [GHC warn about tabs by default](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/options-sanity.html), because they [have](https://stackoverflow.com/questions/35853689/parse-error-on-input-function-haskell) [been](https://stackoverflow.com/questions/35060673/parse-error-in-nested-if-do-blocks) [a](https://stackoverflow.com/questions/33480140/parse-error-in-pattern-putstrln-possibly-caused-by-a-missing-do) [source](https://stackoverflow.com/questions/24842552/haskell-parse-error-on-input) [of](https://stackoverflow.com/questions/24372766/parse-error-in-valid-code) [many](https://stackoverflow.com/questions/16870038/haskell-syntax-error-for-where-statement) parser errors in the past, so you probably want to get rid of them too.

See also
--------

*   [A reddit thread on the topic](https://www.reddit.com/r/haskell/comments/15gz8q/a_nondirty_shot_at_tabs_vs_spaces/) (majority pro spaces, but some pro tabs)
*   [Good Haskell Style](http://urchin.earth.li/~ian/style/haskell.html) (pro spaces)
*   [Yet Another Tabs v Space debate](http://dmwit.com/tabs/) (pro mixing)
    