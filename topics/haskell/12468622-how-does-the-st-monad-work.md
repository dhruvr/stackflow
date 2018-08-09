
# How does the ST monad work?

## Question
        
I understand that the ST monad is something like a little brother of IO, which in turn is the state monad with added `RealWorld` magic. I can picture states and I can picture that RealWorld is somehow put into IO, but every time I write a type signature of `ST` the `s` of the ST monad confuses me.

Take, for example, `ST s (STArray s a b)`. How does the `s` work there? Is it just used to build up some artificial data dependency between computations without being able to be referenced like states in the state monad (due to the `forall`)?

I'm just throwing out ideas and would really appreciate someone more knowledgeable than I to explain it to me.

## Answer
        
The `s` keeps objects inside the `ST` monad from leaking to the outside of the `ST` monad.

    -- This is an error... but let's pretend for a moment...
    let a = runST $ newSTRef (15 :: Int)
        b = runST $ writeSTRef a 20
        c = runST $ readSTRef a
    in b `seq` c
    

Okay, this is a type error (which is a good thing! we don't want `STRef` to leak outside the original computation!). It's a type error because of the extra `s`. Remember that `runST` has the signature:

    runST :: (forall s . ST s a) -> a
    

This means that the `s` on the computation that you're running has to have no constraints on it. So when you try to evaluate `a`:

    a = runST (newSTRef (15 :: Int) :: forall s. ST s (STRef s Int))
    

The result would have type `STRef s Int`, which is wrong since the `s` has "escaped" outside of the `forall` in `runST`. Type variables always have to appear on the inside of a `forall`, and Haskell allows implicit `forall` quantifiers everywhere. There's simply no rule that allows you to to meaningfully figure out the return type of `a`.

**Another example with `forall`:** To clearly show why you can't allow things to escape a `forall`, here is a simpler example:

    f :: (forall a. [a] -> b) -> Bool -> b
    f g flag =
      if flag
      then g "abcd"
      else g [1,2]
    
    > :t f length
    f length :: Bool -> Int
    
    > :t f id
    -- error --
    

Of course `f id` is an error, since it would return either a list of `Char` or a list of `Int` depending on whether the boolean is true or false. It's simply wrong, just like the example with `ST`.

**On the other hand,** if you didn't have the `s` type parameter then everything would type check just fine, even though the code is obviously pretty bogus.

**How ST actually works:** Implementation-wise, the `ST` monad is actually the same as the `IO` monad but with a slightly different interface. When you use the `ST` monad you actually get `unsafePerformIO` or the equivalent, behind the scenes. The reason you can do this safely is because of the type signature of all `ST`-related functions, especially the part with the `forall`.
