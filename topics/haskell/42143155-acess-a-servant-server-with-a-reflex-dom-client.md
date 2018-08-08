
# Acess a servant server with a reflex-dom client

## Question
      
I'm using version 0.4 of reflex-dom and I have a tiny reflex-dom client:

    {-# LANGUAGE OverloadedStrings #-}
    import Reflex.Dom
    import qualified Data.Text as T
    import Data.Monoid
    
    main :: IO ()
    main = mainWidget body
    
    body :: MonadWidget t m => m ()
    body  = el "div" $ do
      pb <- getPostBuild
      snd <- button "Send"
      -- Use one of the following URL's:
      let defReq = "http://localhost:8080/name/3"
      -- let defReq = "https://api.nasa.gov/planetary/apod?api_key=DEMO_KEY"
      let req = XhrRequest "GET" defReq (def {_xhrRequestConfig_sendData = defReq} )
      let evReq = tagPromptlyDyn (constDyn req) snd
      evRsp <- performRequestAsync evReq
      let evResult = (result . _xhrResponse_responseText) <$> evRsp
      el "p" $ return ()
      dynText =<< holdDyn "NOPE" evResult
      return ()
    
    result :: Show a => Maybe a -> T.Text
    result (Just x) = "Received: " <> T.pack (show x)
    result Nothing = "Response is Nothing"
    

As described in [XhrRequest with reflex/reflex-dom](https://stackoverflow.com/questions/30264504/xhrrequest-with-reflex-reflex-dom), I'm using _\_xhrResponse\_responseText_ and not _decodeXhrResponse_.

When I run this client with the NASA URL, it displays a nice JSON string. Therefore I assume, this reflex-dom client is working.

I have a tiny servant server too:

    {-# LANGUAGE OverloadedStrings #-}
    {-# LANGUAGE DataKinds #-}
    {-# LANGUAGE TypeOperators #-}
    
    import Servant
    import Servant.API
    import Servant.Server
    import Network.Wai
    import Network.Wai.Handler.Warp
    import Network.Wai.Logger       (withStdoutLogger)
    import qualified Data.Text as T
    
    main :: IO ()
    main = withStdoutLogger $ \aplogger -> do
             let settings = setPort 8080 $ setLogger aplogger defaultSettings
             runSettings settings app
    
    app :: Application
    app = serve userAPI server
    
    userAPI :: Proxy API    -- API usage:  http://localhost:8080/name/2
    userAPI = Proxy
    
    type API = "name" :> Capture "pid" Int :> Get '[PlainText] T.Text
    
    server :: Server API
    server =  name
    
    name :: Monad m => Int ->  m T.Text
    name pid = return $ nameById pid
    
    nameById :: Int -> T.Text
    nameById 1 = "Isaac Newton"
    nameById 2 = "Galileo Galilei"
    nameById 3 = "Marie Curie"
    nameById _ = "UNKNOWN!!"
    

When I access this server in the browser with `http://localhost:8080/name/3` or with `curl`, I see the expected result `Marie Curie`. Therefore I assume, this servant server is working.

When I run the above reflex-dom client with the URL of the localhost, I can see the request in the stdout log of the server, but the client does **NOT** display the name of _Marie Curie_. Instead the client just displays an empty string! So as a team, the client and the server do not work together! Why?
## Answer
      
You probably are seeing Cross-Origin Resource Sharing (CORS) problems. You can verify this (in chrome at least) by checking your browser console for an error that looks like this:

> XMLHttpRequest cannot load [http://localhost:8080/name/3](http://localhost:8080/name/3). No 'Access-Control-Allow-Origin' header is present on the requested resource. Origin '[http://localhost:8000](http://localhost:8000)' is therefore not allowed access.

If this is the case, you can enable CORS in your server by replacing this line :

    app = serve userAPI server
    

with this line:

    app = simpleCors (serve userAPI server)
    

You will need to import wai-cors:

    import Network.Wai.Middleware.Cors
    

here is your servant server with these changes:

    {-# LANGUAGE OverloadedStrings #-}
    {-# LANGUAGE DataKinds #-}
    {-# LANGUAGE TypeOperators #-}
    
    import Servant
    import Servant.API
    import Servant.Server
    import Network.Wai
    import Network.Wai.Handler.Warp
    import Network.Wai.Logger       (withStdoutLogger)
    import Network.Wai.Middleware.Cors
    import qualified Data.Text as T
    
    main :: IO ()
    main = withStdoutLogger $ \aplogger -> do
             let settings = setPort 8080 $ setLogger aplogger defaultSettings
             runSettings settings app
    
    app :: Application
    app = simpleCors (serve userAPI server)
    
    userAPI :: Proxy API    -- API usage:  http://localhost:8080/name/2
    userAPI = Proxy
    
    type API = "name" :> Capture "pid" Int :> Get '[PlainText] T.Text
    
    server :: Server API
    server =  name
    
    name :: Monad m => Int ->  m T.Text
    name pid = return $ nameById pid
    
    nameById :: Int -> T.Text
    nameById 1 = "Isaac Newton"
    nameById 2 = "Galileo Galilei"
    nameById 3 = "Marie Curie"
    nameById _ = "UNKNOWN!!"
    