
# What is the Comonad typeclass in Haskell?

## Question
        
What is the Comonad typeclass in Haskell? As in Comonad from [Control.Comonad in the comonad package](http://hackage.haskell.org/packages/archive/comonad/1.1.1.1/doc/html/Control-Comonad.html) (explanations of any other packages that provide a Comonad typeclass are also welcome). I've vaguely heard about Comonad, but all I really know about it is that is provides `extract :: w a -> a`, sort of a parallel to Monad's `return :: a -> m a`.

Bonus points for noting "real life" uses of Comonad in "real" code.

## Answer
        
These links may be helpful:

1.  [Evaluating cellular automata is comonadic](http://blog.sigfpe.com/2006/12/evaluating-cellular-automata-is.html). In particular, "whenever you see large datastructures pieced together from lots of small but similar computations there's a good chance that we're dealing with a comonad".
2.  [Sequences, streams, and segments](http://conal.net/blog/posts/sequences-streams-and-segments)
3.  [Comonads in everyday life](http://fmapfixreturn.wordpress.com/2008/07/09/comonads-in-everyday-life/)
