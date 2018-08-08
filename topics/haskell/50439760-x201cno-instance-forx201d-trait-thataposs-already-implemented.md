
# &#x201C;No instance for&#x201D; trait that&apos;s already implemented

## Question
      
I'd like to use Servant's [`ClientM`](https://hackage.haskell.org/package/servant-client-0.13.0.1/docs/Servant-Client.html#t:ClientM) monad with [`finally`](http://hackage.haskell.org/package/lifted-base-0.2.3.12/docs/Control-Exception-Lifted.html#v:finally)`:: MonadBaseControl IO m => m a -> m b -> m a`, but am faced with error `No instance for (MonadBaseControl IO ClientM)`.

Oddly, this exact instance appears to be [defined](https://hackage.haskell.org/package/servant-client-0.13.0.1/docs/Servant-Client.html#t:ClientM) already, be it from an [internal](https://hackage.haskell.org/package/servant-client-0.13.0.1/docs/src/Servant.Client.Internal.HttpClient.html) module.

Do I need to explicitly import such instances somehow?
## Answer
      
It works fine if, as Daniel said, you import the module that defines the desired instance:

    Prelude> import Control.Exception.Lifted
    Prelude Control.Exception.Lifted> import Servant.Client
    Prelude Control.Exception.Lifted Servant.Client> :set -XTypeApplications
    Prelude Control.Exception.Lifted Servant.Client> :type finally @ ClientM
    finally @ ClientM :: ClientM a -> ClientM b -> ClientM a
    