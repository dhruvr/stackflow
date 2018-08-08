
# Haskell Servant - is there a way to compose API types?

## Question
      
I know this isn't valid syntax, but is there a way to accomplish something like this in servant?

    type StandardAPI = "foo" :> Get '[JSON] Whatever
    
    type CustomAPI = StandardAPI :<|> "customroute" :> Get '[JSON] Blah
    

in other words, composing APIs. In Spock I could do this with the monadic route construction, but I'm not sure how to do this in servant.

This way I can reuse shared routes across APIs. Another reason to use this is that there are certain types that don't work with client generators, such as Raw.
## Answer
      
Yes, referencing [Servant documentation](http://haskell-servant.github.io/tutorial/0.4/server.html#nested-apis) you can use

    type CombinedAPI = "users" :> UsersAPI
              :<|> "products" :> ProductsAPI
    
    server :: Server CombinedAPI
    server = usersServer :<|> productsServer
    
    usersServer :: Server UsersAPI
    usersServer = -- implementation
    
    productsServer :: Server ProductsAPI
    productsServer = -- implementation
    