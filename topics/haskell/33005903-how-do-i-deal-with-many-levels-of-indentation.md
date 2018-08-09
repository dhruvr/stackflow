
# How do I deal with many levels of indentation?

## Question
        
I am writing a script that has a very logically complicated loop:

    main = do
        inFH <- openFile "..." ReadMode
        outFH <- openFile "..." WriteMode
    
        forM myList $ \ item ->
            ...
            if ... 
                then ...
                else do
                    ...
                    case ... of
                        Nothing -> ...
                        Just x  -> do
                            ...
                                ...
    

The code soon flies to the right, so I was thinking breaking it into pieces, using for example `where` clauses. The problem is, many of these `...` contain reading/writing statements to the two handles `inFH` and `outFH`, and using a `where` statement will render those two names out of context. I would have to send in these two variables everytime I use a `where` statement.

Is there a better way of dealing with this?

## Answer
        
In many cases, these deeply-nested indentations are the result of deeply-nested error checking. If that's so for you, you should look into `MaybeT` and its big brother `ExceptT`. These offer a clean way to separate the "what do we do when something went wrong" code from the "what do we do assuming everything goes right" code. In your example, I might write:

    data CustomError = IfCheckFailed | MaybeCheckFailed
    
    main = handleErrors <=< runExceptT $ do
        inFH  <- liftIO $ openFile ...
        outFH <- liftIO $ openFile ...
        forM myList $ \item -> do
            when (...) (throwError IfCheckFailed)
            ...
            x <- liftMaybe MaybeCheckFailed ...
            ...
    
    liftMaybe :: MonadError e m => e -> Maybe a -> m a
    liftMaybe err = maybe (throwError err) return
    
    handleErrors :: Either CustomError a -> IO a
    handleErrors (Left err) = case err of
        IfCheckFailed    -> ...
        MaybeCheckFailed -> ...
    handleErrors (Right success) = return success
    

Notice that we still increase indentation at the `forM` loop; but the other checks are done "in-line" in `main`, and are handled all at the same indentation level in `handleErrors`.
