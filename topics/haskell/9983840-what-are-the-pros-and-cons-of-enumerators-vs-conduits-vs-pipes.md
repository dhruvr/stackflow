
# What are the pros and cons of Enumerators vs. Conduits vs. Pipes?

## Question
        
I'd like to hear from someone with a deeper understanding than myself what the fundamental differences are between [Enumerators](http://hackage.haskell.org/package/enumerator), [Conduits](http://hackage.haskell.org/package/conduit), and [Pipes](http://hackage.haskell.org/package/pipes) as well as the key benefits and drawbacks. Some [discussion's already](http://twanvl.nl/blog/haskell/conduits-vs-pipes) [ongoing](http://www.reddit.com/r/haskell/comments/rhs0y/summarizing_the_conduit_questions/c461j36) but it'd be nice to have a high-level overview.

## Answer
        
Enumerators/Iteratees as an abstraction were invented by Oleg Kiselyov. They provide a clean way of doing IO with predictable (low) resource requirements. The current Enumerators package is pretty close to Oleg's original work.

Conduits were created for the Yesod web framework. My understanding is that they were designed to be blazingly fast. Early versions of the library were highly stateful.

Pipes focus on elegance. They have just one type instead of several, form monad (transformer) and category instances, and are very "functional" in design.

If you like categorical explanations: the `Pipe` type is just the free monad over the following ungodly simple functor

    data PipeF a b m r = M (m r) | Await (a -> r) | Yield b r
    instance Monad m => Functor (PipeF a b m) where
       fmap f (M mr) = M $ liftM mr
       fmap f (Await g) = Await $ f . g
       fmap f (Yield b p) = Yield b (f p)
    --Giving:
    newtype Pipe a b m r = Pipe {unPipe :: Free (PipeF a b m) r}
      deriving (Functor, Applicative, Monad)
    
    --and
    instance MonadTrans (Pipe a b) where
       lift = Pipe . inj . M
    

In the actual pipe definition these are baked in, but the simplicity of this definition is amazing. Pipes form a category under the operation `(<+<) :: Monad m => Pipe c d m r -> Pipe a b m r -> Pipe a d m r` which takes whatever the first pipe `yields` and feeds it to the awaiting second pipe.

It looks like `Conduits` is moving to be more `Pipe` like (using CPS instead of state, and switching to a single type) while Pipes are gaining support for better error handling, and perhaps the return of separate types for generators and consumers.

This area is moving quickly. I've been hacking on an experimental variant of the Pipe library with these features, and know other people are as well (see the Guarded Pipes package on Hackage), but suspect that Gabriel (the author of Pipes) will figure them out before I do.

My recommendations: if you are using Yesod, use Conduits. If you want a mature library use Enumerator. If you primarily care about elegance, use Pipe.
