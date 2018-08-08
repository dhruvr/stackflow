
# Use of `get` and `toSqlKey` in persistent

## Question
      
I'm trying to use persistent-postgresql with servant.

I have a `User` model.

I want to have an endpoint that takes an id and returns the user with that id.

According to other answers I can use `toSqlKey` to turn an `Int64` into a `Key` to feed to `get`.

My function looks like:

    oneUser :: Int64 -> App (Entity User)
    oneUser userId = do
      maybeUser <- runDb $ get $ toSqlKey userId
      case maybeUser of
        Nothing ->
          throwError err404
        Just user ->
          return user
    

When I try to compile I get the error `Couldn't match expected type ‘PersistEntityBackend (Entity User)’ with actual type ‘SqlBackend’`

Use of `selectList` works fine.

    allUsers :: App [Entity User]
    allUsers = runDb $ selectList [] []
    

Please tell me what I'm doing wrong and where I should look in the future for stuff like this. I couldn't find `get` on hackage/the right version of the library on stackage etc.

`runDb` looks like:

    runDb :: (MonadReader Config m, MonadIO m) => SqlPersistT IO b -> m b
    runDb query = do
      pool <- asks getPool
      liftIO $ runSqlPool query pool
    

taken from [this github project](https://github.com/parsonsmatt/servant-persistent/blob/master/src/Models.hs).
## Answer
      
The difference is that `get ...` returns a plain `User` not an `Entity User`, so this will work:

    altSingleUser :: Int64 -> App User
    altSingleUser userid = do
        let foo = get (toSqlKey userid) :: SqlPersistT IO (Maybe User)
        maybeUser <- runDb $ foo
        case maybeUser of
             Nothing ->
                throwError err404
             Just person ->
                return person
    

If you want to return an `Entity User`, just change the last return statement to:

    return $ Entity { entityKey = toSqlKey userid, entityVal = person }
    