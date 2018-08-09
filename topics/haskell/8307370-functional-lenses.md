
# Functional lenses

## Question
        
Could someone explain functional lenses to me? It's a surprisingly difficult subject to google for and I haven't made any progress. All I know is that they provide similar get/set functionality than in OO.

## Answer
        
A lens consists of two functions, a getter and a setter:

    data Lens a b = Lens { getter :: a -> b, setter :: b -> a -> a }
    

For example, we might have lenses for the first and second parts of a pair:

    fstLens :: Lens (a, b) a
    fstLens = Lens fst $ \x (a, b) -> (x, b)
    
    sndLens :: Lens (a, b) b
    sndLens = Lens snd $ \x (a, b) -> (a, x)
    

The real convenience of lenses is that they compose:

    compose :: Lens b c -> Lens a b -> Lens a c
    compose f g = Lens (getter f . getter g) $
                       \c a -> setter g (setter f c (getter g a)) a
    

And they mechanically convert to `State` transitions:

    lensGet :: MonadState s m => Lens s a -> m a
    lensGet = gets . getter
    
    lensSet :: MonadState s m => Lens s b -> b -> m ()
    lensSet f = modify . setter f
    
    lensMod :: MonadState s m => Lens s b -> (b -> b) -> m ()
    lensMod f g = modify $ setter f =<< g . getter f
    
    (+=) :: (MonadState s m, Num b) => Lens s b -> b -> m ()
    f += x = lensMod f (+ x)
