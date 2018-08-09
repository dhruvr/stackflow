
# Good Haskell source to read and learn from [closed]

## Question
        
What are some open source programs that use Haskell and can be considered to be _good quality modern Haskell_? The larger the code base, the better.

I want to learn from their source code. I feel I'm past the point of learning from small code examples, which are often to esoteric and small-world. I want to see how code is structured, how monads interact when you have a lot of things going on (logging, I/O, configuration, etc.).

## Answer
        
What I recommend.

_Read code by people from different grad schools in the 1990s_

*   [Oxford style](http://hackage.haskell.org/package/hinze-streams)
*   [Glasgow style](http://hackage.haskell.org/package/monad-par) or ([this](http://hackage.haskell.org/package/hpc))
*   [Chalmers style](http://hackage.haskell.org/package/pretty-1.0.1.2) (or [this](http://hackage.haskell.org/package/chalmers-lava2000))
*   [York style](http://hackage.haskell.org/package/smallcheck)
*   [Portland style](http://hackage.haskell.org/package/extcore-1.0.1) or [OGI style](http://hackage.haskell.org/package/orc) (or [this](http://hackage.haskell.org/package/mtl-1.0))
*   [Utrecht style](http://hackage.haskell.org/package/uu-parsinglib)
*   [Yale style](http://hackage.haskell.org/package/haskore-vintage-0.1)
*   Special case: [CMU/Elliott](http://hackage.haskell.org/package/unamb)

_Read code by the old masters certain people (incomplete list)_

*   [Marlow](http://simonmar.github.io/); [Paterson](http://www.soi.city.ac.uk/~ross/); [Peyton Jones](http://research.microsoft.com/en-us/people/simonpj/); [Gill](http://www.ittc.ku.edu/csdl/fpg/Users/AndyGill); [Launchbury](http://corp.galois.com/john-launchbury/); [Hughes](http://www.chalmers.se/cse/EN/people/hughes-john); [Wadler](http://homepages.inf.ed.ac.uk/wadler/); [Bird](http://www.cs.ox.ac.uk/people/richard.bird/); [Claessen](http://www.cse.chalmers.se/~koen/); [Jones](http://web.cecs.pdx.edu/~mpj/); [Tolmach](http://web.cecs.pdx.edu/~apt/); [Sheard](http://web.cecs.pdx.edu/~sheard/); [Swiestra](http://www.cs.uu.nl/staff/doaitse.html); [Augustsson](http://augustss.blogspot.com/); [Runciman](http://www-users.cs.york.ac.uk/~colin/); [Wallace](http://www.haskellers.com/user/malcolm); [Thompson](http://www.cs.kent.ac.uk/people/staff/sjt/); [Hinze](http://www.cs.ox.ac.uk/ralf.hinze/); [Gibbons](http://www.cs.ox.ac.uk/jeremy.gibbons/); [Leijen](http://research.microsoft.com/en-us/um/people/daan/); [Hudak](http://haskell.cs.yale.edu/people/paul-hudak/); [Elliott](http://conal.net/); [Finne](http://haskell.forkio.com/); [Chakravarty](http://www.cse.unsw.edu.au/~chak/); and
*   Anyone who has written a [functional pearl](http://www.haskell.org/haskellwiki/Research_papers/Functional_pearls).

Note that people like me, Coutts, Mitchell, O'Sullivan, Lynagh, etc. learned our Haskell style from these guys.

_Read some applications_

*   Read the GHC [base library source](http://hackage.haskell.org/package/base)
*   Read the [xmonad source](http://hackage.haskell.org/package/xmonad)
