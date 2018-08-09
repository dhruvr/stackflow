
# Implementing a language interpreter in Haskell

## Question
        
I want to implement an imperative language interpreter in Haskell (for educational purposes). But it's difficult for me to create right architecture for my interpreter: How should I store variables? How can I implement nested function calls? How should I implement variable scoping? How can I add debugging possibilities in my language? Should I use monads/monad transformers/other techniques? etc.

Does anybody know good articles/papers/tutorials/sources on this subject?

## Answer
        
If you are new to writing this kind of processors, I would recommend to put off using monads for a while and first focus on getting a barebones implementation without any bells or whistles.

The following may serve as a minitutorial.

I assume that you have already tackled the issue of parsing the source text of the programs you want to write an interpreter for and that you have some types for capturing the abstract syntax of your language. The language that I use here is very simple and only consists of integer expressions and some basic statements.

Preliminaries
-------------

Let us first import some modules that we will use in just a bit.

    import Data.Function
    import Data.List
    

The essence of an imperative language is that it has some form of mutable variables. Here, variables simply represented by strings:

    type Var = String
    

Expressions
-----------

Next, we define expressions. Expressions are constructed from integer constants, variable references, and arithmetic operations.

    infixl 6 :+:, :-:
    infixl 7 :*:, :/:
    
    data Exp
      = C Int        -- constant                                                     
      | V Var        -- variable                                                     
      | Exp :+: Exp  -- addition                                                     
      | Exp :-: Exp  -- subtraction                                                  
      | Exp :*: Exp  -- multiplication                                               
      | Exp :/: Exp  -- division
    

For example, the expression that adds the constant 2 to the variable `x` is represented by `V "x" :+: C 2`.

Statements
----------

The statement language is rather minimal. We have three forms of statements: variable assignments, while loops, and sequences.

    infix 1 :=
    
    data Stmt
      = Var := Exp      -- assignment                                                
      | While Exp Stmt  -- loop                                                      
      | Seq [Stmt]      -- sequence
    

For example, a sequence of statements for "swapping" the values of the variables `x` and `y` can be represented by `Seq ["tmp" := V "x", "x" := V "y", "y" := V "tmp"]`.

Programs
--------

A program is just a statement.

    type Prog = Stmt
    

Stores
------

Now, let us move to the actual interpreter. While running a program, we need to keep track of the values assigned to the different variables in the programs. These values are just integers and as a representation of our "memory" we just use lists of pairs consisting of a variable and a value.

    type Val = Int
    type Store = [(Var, Val)]
    

Evaluating expressions
----------------------

Expressions are evaluated by mapping constants to their value, looking up the values of variables in the store, and mapping arithmetic operations to their Haskell counterparts.

    eval :: Exp -> Store -> Val
    eval (C n) r       = n
    eval (V x) r       = case lookup x r of
                           Nothing -> error ("unbound variable `" ++ x ++ "'")
                           Just v  -> v
    eval (e1 :+: e2) r = eval e1 r + eval e2 r
    eval (e1 :-: e2) r = eval e1 r - eval e2 r
    eval (e1 :*: e2) r = eval e1 r * eval e2 r
    eval (e1 :/: e2) r = eval e1 r `div` eval e2 r
    

Note that if the store contains multiple bindings for a variable, `lookup` selects the bindings that comes first in the store.

Executing statements
--------------------

While the evaluation of an expression cannot alter the contents of the store, executing a statement may in fact result in an update of the store. Hence, the function for executing a statement takes a store as an argument and produces a possibly updated store.

    exec :: Stmt -> Store -> Store
    exec (x := e) r                    = (x, eval e r) : r
    exec (While e s) r | eval e r /= 0 = exec (Seq [s, While e s]) r
                       | otherwise     = r
    exec (Seq []) r                    = r
    exec (Seq (s : ss)) r              = exec (Seq ss) (exec s r)
    

Note that, in the case of assignments, we simply push a new binding for the updated variable to the store, effectively shadowing any previous bindings for that variable.

Top-level Interpreter
---------------------

Running a program reduces to executing its top-level statement in the context of an initial store.

    run :: Prog -> Store -> Store
    run p r = nubBy ((==) `on` fst) (exec p r)
    

After executing the statement we clean up any shadowed bindings, so that we can easily read off the contents of the final store.

Example
-------

As an example, consider the following program that computes the Fibonacci number of the number stored in the variable `n` and stores its result in the variable `x`.

    fib :: Prog
    fib = Seq
      [ "x" := C 0
      , "y" := C 1
      , While (V "n") $ Seq
          [ "z" := V "x" :+: V "y"
          , "x" := V "y"
          , "y" := V "z"
          , "n" := V "n" :-: C 1
          ]
      ]
    

For instance, in an interactive environment, we can now use our interpreter to compute the 25th Fibonacci number:

    > lookup "x" $ run fib [("n", 25)]
    Just 75025
    

Monadic Interpretation
----------------------

Of course, here, we are dealing with a very simple and tiny imperative language. As your language gets more complex, so will the implementation of your interpreter. Think for example about what additions you need when you add procedures and need to distinguish between local (stack-based) storage and global (heap-based) storage. Returning to that part of your question, you may then indeed consider the introduction of monads to streamline the implementation of your interpreter a bit.

In the example interpreter above, there are two "effects" that are candidates for being captured by a monadic structure:

1.  The passing around and updating of the store.
2.  Aborting running the program when a run-time error is encountered. (In the implementation above, the interpreter simply crashes when such an error occurs.)

The first effect is typically captured by a state monad, the second by an error monad. Let us briefly investigate how to do this for our interpreter.

We prepare by importing just one more module from the standard libraries.

    import Control.Monad
    

We can use monad transformers to construct a composite monad for our two effects by combining a basic state monad and a basic error monad. Here, however, we simply construct the composite monad in one go.

    newtype Interp a = Interp { runInterp :: Store -> Either String (a, Store) }
    
    instance Monad Interp where
      return x = Interp $ \r -> Right (x, r)
      i >>= k  = Interp $ \r -> case runInterp i r of
                   Left msg      -> Left msg
                   Right (x, r') -> runInterp (k x) r'
      fail msg = Interp $ \_ -> Left msg
    

For reading from and writing to the store, we introduce effectful functions `rd` and `wr`:

    rd :: Var -> Interp Val
    rd x = Interp $ \r -> case lookup x r of
             Nothing -> Left ("unbound variable `" ++ x ++ "'")
             Just v  -> Right (v, r)
    
    wr :: Var -> Val -> Interp ()
    wr x v = Interp $ \r -> Right ((), (x, v) : r)
    

Note that `rd` produces a `Left`-wrapped error message if a variable lookup fails.

The monadic version of the expression evaluator now reads

    eval :: Exp -> Interp Val
    eval (C n)       = do return n
    eval (V x)       = do rd x
    eval (e1 :+: e2) = do v1 <- eval e1
                          v2 <- eval e2
                          return (v1 + v2)
    eval (e1 :-: e2) = do v1 <- eval e1
                          v2 <- eval e2
                          return (v1 - v2)
    eval (e1 :*: e2) = do v1 <- eval e1
                          v2 <- eval e2
                          return (v1 * v2)
    eval (e1 :/: e2) = do v1 <- eval e1
                          v2 <- eval e2
                          if v2 == 0
                            then fail "division by zero"
                            else return (v1 `div` v2)
    

In the case for `:/:`, division by zero results in an error message being produced through the `Monad`-method `fail`, which, for `Interp`, reduces to wrapping the message in a `Left`-value.

For the execution of statements we have

    exec :: Stmt -> Interp ()
    exec (x := e)       = do v <- eval e
                             wr x v
    exec (While e s)    = do v <- eval e
                             when (v /= 0) (exec (Seq [s, While e s]))
    exec (Seq [])       = do return ()
    exec (Seq (s : ss)) = do exec s
                             exec (Seq ss)
    

The type of `exec` conveys that statements do not result in values but are executed only for their effects on the store or the run-time errors they may trigger.

Finally, in the function `run` we perform a monadic computation and process its effects.

    run :: Prog -> Store -> Either String Store
    run p r = case runInterp (exec p) r of
                Left msg      -> Left msg
                Right (_, r') -> Right (nubBy ((==) `on` fst) r')
    

In the interactive environment, we can now revisit the interpretation of our example program:

    > lookup "x" `fmap` run fib [("n", 25)]
    Right (Just 75025)
    
    > lookup "x" `fmap` run fib []
    Left "unbound variable `n'"
