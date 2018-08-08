
# Issues using pattern matching with servant-client

## Question
      
In the [Servant docs](http://haskell-servant.readthedocs.io/en/stable/tutorial/Client.html), we have the following api:

    type API = "position" :> Capture "x" Int :> Capture "y" Int :> Get '[JSON] Position
          :<|> "hello" :> QueryParam "name" String :> Get '[JSON] HelloMessage
          :<|> "marketing" :> ReqBody '[JSON] ClientInfo :> Post '[JSON] Email
    

and we can define client functions like so:

    api :: Proxy API
    api = Proxy
    
    position :<|> hello :<|> marketing = client api
    

If our api type instead looked like:

    type API = QueryParam "test" Int :> (
        "position" :> Capture "x" Int :> Capture "y" Int :> Get '[JSON] Position
          :<|> "hello" :> QueryParam "name" String :> Get '[JSON] HelloMessage
          :<|> "marketing" :> ReqBody '[JSON] ClientInfo :> Post '[JSON] Email)
    

which is identical to the original api but with an additional "test" query parameter for all endpoints, how would we obtain our client functions? I have tried several variants of pattern matching, but to no avail.

If all else, fails, the "test" query parameter could be repeated in the api type for each endpoint, but this is Haskell, we try to avoid repetition.
## Answer
      
Servant API definitions live at the type level. If we want to manipulate them, we need something like a function that transforms types (not values, types themselves) into other types.

The closest thing to such a type-level function in Haskell is called a [closed type family](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#closed-type-families):

    {-# LANGUAGE TypeFamilies #-}
    
    type family PrependParam api where
        PrependParam (a :<|> b) = PrependParam a :<|> PrependParam b
        PrependParam leaf = QueryParam "test" Int :> leaf
    
    type API' = PrependParam API
    

`:<|>` separates routes, while `:>` separates components within a route. We are mapping over the tree of routes and adding a prefix to each one.

We can check that it works from _ghci_, using the `kind!`command:

    ghci> :kind! API'
    API' :: *
    = (QueryParam "test" Int
       :> ("position"
           :> (Capture "x" Int :> (Capture "y" Int :> Get '[JSON] Position))))
      :<|> ((QueryParam "test" Int
             :> ("hello"
                 :> (QueryParam "name" String :> Get '[JSON] HelloMessage)))
            :<|> (QueryParam "test" Int
                  :> ("marketing"
                      :> (ReqBody '[JSON] ClientInfo :> Post '[JSON] Email))))
    