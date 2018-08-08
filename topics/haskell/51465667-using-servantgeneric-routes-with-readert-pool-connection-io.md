
# Using Servant.Generic routes with ReaderT (Pool Connection) IO

## Question
      
I was using `servant-generic-0.1.0.3` and `servant-server-0.13.0.1` to do the following:

    data Site route = Site
      { page :: route :-
          "page" :> Capture "x" Int :> Capture "y" Int :> Get '[JSON] [Int]
      , home :: route :-
          Raw
      } deriving (Generic)
    
    type API = ToServant (Site AsApi)
    
    siteServer :: Pool Connection -> Site AsServer
    siteServer pool = Site
      { page = \x y ->
          liftIO $ withResource pool $ \conn -> someDbFunction conn x y
      , home = serveDirectoryWebApp "static"
      }
    
    api :: Proxy API
    api = Proxy
    
    app :: Pool Connection -> Application
    app pool = serve api (toServant $ siteServer pool)
    

That worked fine, then I tried to use `ReaderT` to avoid passing `Pool Connection` to `siteServer`, so I added `AppM` and replaced `siteServer` like this:

    type AppM = ReaderT (Pool Connection) IO
    
    siteServer :: ServerT API AppM
    siteServer = Site
      { page = do
          pool <- ask
          \x y ->
            liftIO $ withResource pool $ \conn -> someDbFunction conn x y
      , home = serveDirectoryWebApp "static"
      }
    

but I got a bunch of errors when I tried to compile it.

I followed the same steps shown in the [servant cookbook](https://haskell-servant.readthedocs.io/en/stable/cookbook/using-custom-monad/UsingCustomMonad.html), but I couldn't make this work with generic routes, although it works when using regular routes.

Am I missing something that could make this work?
## Answer
      
At least for the record-style routes supported by servant-* >= 0.14 (see [here](https://hackage.haskell.org/package/servant-server-0.14.1/docs/Servant-Server-Generic.html)), if you want to work with another monad than `Handler`, you will want to look at `AsServerT` and `genericServerT`.

Applied to your example, this means `siteServer` should be defined as follows (not typechecked, but should be very close to correct).

    siteServer :: Site (AsServerT AppM)
    siteServer = Site
      { page = ... something in AppM ...
      , home = ... something in AppM ...
      }
    
    -- turning that into a usual chain of :<|>-separated handlers
    oldStyleServer :: ServerT API AppM
    oldStyleServer = genericServerT siteServer
    
    -- bringing it all back in Handler
    oldStyleServerInHandler :: Pool Connection -> Server API -- same as ServerT API Handler
    oldStyleServerInHandler conns = hoistServer (Proxy @API) appToHandler oldStyleServer
      where appToHandler = liftIO . flip runReaderT conns
            -- or something along those lines
    
    -- serving it
    app :: Pool Connection -> Application
    app conns = serve (Proxy @API) (oldStyleServerInHandler conns)
    

**Edit:** Since you're using servant-* < 0.14 with servant-generic, you should replace `genericServerT` with [`toServant`](https://hackage.haskell.org/package/servant-generic-0.1.0.3/docs/Servant-Generic.html#v:toServant).
    