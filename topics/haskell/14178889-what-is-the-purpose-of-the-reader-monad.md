
# What is the purpose of the Reader Monad?

## Question
        
The Reader Monad is so complex and seems to be useless. In imperative language like Java or C++, there is no equivalent term for reader monad (if I am right).

Can you give me a simple example and make me clear a little bit?

## Answer
        
Don't be scared! The reader monad is actually not so complicated, and has real easy-to-use utility.

There are two ways of approaching a monad: we can ask

1.  What does the monad **_do_**? What operations is it equipped with? What is it good for?
2.  How is the monad implemented? From where does it arise?

From the first approach, the reader monad is some abstract type

    data Reader env a
    

such that

    -- Reader is a monad
    instance Monad (Reader env)
    
    -- and we have a function to get its environment
    ask :: Reader env env
    
    -- finally, we can run a Reader
    runReader :: Reader env a -> env -> a
    

So how do we use this? Well, the reader monad is good for passing (implicit) configuration information through a computation.

Any time you have a "constant" in a computation that you need at various points, but really you would like to be able to perform the same computation with different values, then you should use a reader monad.

Reader monads are also used to do what the OO people call [dependency injection](http://en.wikipedia.org/wiki/Dependency_injection). For example, the [negamax](http://en.wikipedia.org/wiki/Negamax) algorithm is used frequently (in highly optimized forms) to compute the value of a position in a two player game. The algorithm itself though does not care what game you are playing, except that you need to be able to determine what the "next" positions are in the game, and you need to be able to tell if the current position is a victory position.

     import Control.Monad.Reader
    
     data GameState = NotOver | FirstPlayerWin | SecondPlayerWin | Tie
    
     data Game position
       = Game {
               getNext :: position -> [position],
               getState :: position -> GameState
              }
    
     getNext' :: position -> Reader (Game position) [position]
     getNext' position
       = do game <- ask
            return $ getNext game position
    
     getState' :: position -> Reader (Game position) GameState
     getState' position
       = do game <- ask
            return $ getState game position
    
    
     negamax :: Double -> position -> Reader (Game position) Double
     negamax color position
         = do state <- getState' position 
              case state of
                 FirstPlayerWin -> return color
                 SecondPlayerWin -> return $ negate color
                 Tie -> return 0
                 NotOver -> do possible <- getNext' position
                               values <- mapM ((liftM negate) . negamax (negate color)) possible
                               return $ maximum values
    

This will then work with any finite, deterministic, two player game.

This pattern is useful even for things that are not really dependency injection. Suppose you work in finance, you might design some complicated logic for pricing an asset (a derivative say), which is all well and good and you can do without any stinking monads. But then, you modify your program to deal with multiple currencies. You need to be able to convert between currencies at the fly. Your first attempt is to define a top level function

    type CurrencyDict = Map CurrencyName Dollars
    currencyDict :: CurrencyDict
    

to get spot prices. You can then call this dictionary in your code....but wait! That won't work! The currency dictionary is immutable and so has to be the same not only for the life of your program, but from the time it gets **compiled**! So what do you do? Well one option would be to use the Reader monad:

     computePrice :: Reader CurrencyDict Dollars
     computePrice
        = do currencyDict <- ask
             --insert computation here
    

Perhaps the most classic use-case is in implementing interpreters. But, before we look at that, we need to introduce another function

     local :: (env -> env) -> Reader env a -> Reader env a
    

Okay, so Haskell and other functional languages are based on the [lambda calculus](http://en.wikipedia.org/wiki/Lambda_calculus). Lambda calculus has a syntax that looks like

     data Term = Apply Term Term | Lambda String Term | Var Term deriving (Show)
    

and we want to write an evaluator for this language. To do so, we will need to keep track of an environment, which is a list of bindings associated with terms (actually it will be closures because we want to do static scoping).

     newtype Env = Env ([(String,Closure)])
     type Closure = (Term,Env)
    

When we are done we should get out a value (or an error):

     data Value = Lam String Closure | Failure String
    

So, lets write the interpreter:

    interp' :: Term -> Reader Env Value
    --when we have lambda term, we can just return it
    interp' (Lambda nv t) 
       = do env <- ask
            return $ Lam nv (t,env)
    --when we run into a value we look it up in the environment
    interp' (Var v) 
       = do (Env env) <- ask
            case lookup (show v) env of
              -- if it is not in the environment we have a problem
              Nothing -> return . Failure $ "unbound variable: " ++ (show v)
              -- if it is in the environment, than we should interpret it
              Just (term,env) -> local (const env) $ interp' term
    --the complicated case is an application
    interp' (Apply t1 t2)
       = do v1 <- interp' t1
            case v1 of
               Failure s -> return (Failure s)
               Lam nv clos -> local (\(Env ls) -> Env ((nv,clos):ls)) $ interp' t2
    --I guess not that complicated!
    

Finally, we can use it by passing a trivial environment:

    interp :: Term -> Value
    interp term = runReader (interp' term) (Env [])
    

And that is it. A fully functional interpreter for the lambda calculus.

* * *

So, the other way to think about this is to ask: how is it implemented? Well the answer is that the reader monad is actually one of the simplest and most elegant of all monads.

    newtype Reader env a = Reader {runReader :: env -> a}
    

Reader is just a fancy name for functions! We have already defined `runReader` so what about the other parts of the API? Well every `Monad` is also a `Functor`:

    instance Functor (Reader env) where
       fmap f (Reader g) = Reader $ f . g
    

Now, to get a monad:

    instance Monad (Reader env) where
       return x = Reader (\_ -> x)
       (Reader f) >>= g = Reader $ \x -> runReader (g (f x)) x
    

which is not so scary. `ask` is really simple:

    ask = Reader $ \x -> x
    

while `local` isn't so bad.

    local f (Reader g) = Reader $ \x -> runReader g (f x)
    

Okay, so the reader monad is just a function. Why have Reader at all? Good question. Actually, you don't need it!

    instance Functor ((->) env) where
       fmap = (.)
    
     instance Monad ((->) env) where
       return = const
       f >>= g = \x -> g (f x) x
    

These are even simpler. What is more, `ask` is just `id` and `local` is just function composition in the other order!
