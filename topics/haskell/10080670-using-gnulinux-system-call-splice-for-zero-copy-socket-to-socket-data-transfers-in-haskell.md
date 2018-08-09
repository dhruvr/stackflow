
# Using GNU/Linux system call `splice` for zero-copy Socket to Socket data transfers in Haskell

## Question
        
**Update: Mr. Nemo's answer helped solve the problem! The code below contains the fix! See the `nb False` and `nb True` calls below.**

**There is also a new Haskell package called [`splice`](http://hackage.haskell.org/package/splice)** (, which has OS-specific and portable implementations of best known socket to socket data transfer loops)**.**

I have the following (Haskell) code:

    #ifdef LINUX_SPLICE
    #include <fcntl.h>
    {-# LANGUAGE CPP #-}
    {-# LANGUAGE ForeignFunctionInterface #-}
    #endif
    
    module Network.Socket.Splice (
        Length
      , zeroCopy
      , splice
    #ifdef LINUX_SPLICE
      , c_splice
    #endif
      ) where
    
    import Data.Word
    import Foreign.Ptr
    
    import Network.Socket
    import Control.Monad
    import Control.Exception
    import System.Posix.Types
    import System.Posix.IO
    
    #ifdef LINUX_SPLICE
    import Data.Int
    import Data.Bits
    import Unsafe.Coerce
    import Foreign.C.Types
    import Foreign.C.Error
    import System.Posix.Internals
    #else
    import System.IO
    import Foreign.Marshal.Alloc
    #endif
    
    
    zeroCopy :: Bool
    zeroCopy =
    #ifdef LINUX_SPLICE
      True
    #else
      False
    #endif
    
    
    type Length =
    #ifdef LINUX_SPLICE
      (#type size_t)
    #else
      Int
    #endif
    
    
    -- | The 'splice' function pipes data from
    --   one socket to another in a loop.
    --   On Linux this happens in kernel space with
    --   zero copying between kernel and user spaces.
    --   On other operating systems, a portable
    --   implementation utilizes a user space buffer
    --   allocated with 'mallocBytes'; 'hGetBufSome'
    --   and 'hPut' are then used to avoid repeated 
    --   tiny allocations as would happen with 'recv'
    --   'sendAll' calls from the 'bytestring' package.
    splice :: Length -> Socket -> Socket -> IO ()
    splice l (MkSocket x _ _ _ _) (MkSocket y _ _ _ _) = do
    
      let e  = error "splice ended"
    
    #ifdef LINUX_SPLICE
    
      (r,w) <- createPipe
      print ('+',r,w)
      let s  = Fd x -- source
      let t  = Fd y -- target
      let c  = throwErrnoIfMinus1 "Network.Socket.Splice.splice"
      let u  = unsafeCoerce :: (#type ssize_t) -> (#type size_t)
      let fs = sPLICE_F_MOVE .|. sPLICE_F_MORE
      let nb v = do setNonBlockingFD x v
                    setNonBlockingFD y v
      nb False
      finally
        (forever $ do 
           b <- c $ c_splice s nullPtr w nullPtr    l  fs
           if b > 0
             then   c_splice r nullPtr t nullPtr (u b) fs)
             else   e
        (do closeFd r
            closeFd w
            nb True
            print ('-',r,w))
    
    #else
    
      -- ..    
    
    #endif
    
    
    #ifdef LINUX_SPLICE
    -- SPLICE
    
    -- fcntl.h
    -- ssize_t splice(
    --   int          fd_in,
    --   loff_t*      off_in,
    --   int          fd_out,
    --   loff_t*      off_out,
    --   size_t       len,
    --   unsigned int flags
    -- );
    
    foreign import ccall "splice"
      c_splice
      :: Fd
      -> Ptr (#type loff_t)
      -> Fd
      -> Ptr (#type loff_t)
      -> (#type size_t)
      -> Word
      -> IO (#type ssize_t)
    
    sPLICE_F_MOVE :: Word
    sPLICE_F_MOVE = (#const "SPLICE_F_MOVE")
    
    sPLICE_F_MORE :: Word
    sPLICE_F_MORE = (#const "SPLICE_F_MORE")
    #endif
    

**Note:** _The code above now **just works!** Below is no longer valid thanks to Nemo!_

I call `splice` as defined above with two open and connected sockets (which are already used to transmit minimal amount of handshake data using either the sockets API `send` and `recv` calls or converted to handles and used with `hGetLine` and `hPut`) and I keep getting:

    Network.Socket.Splice.splice: resource exhausted (Resource temporarily unavailable)
    

at the first `c_splice` call site: `c_splice` returns `-1` and sets some `errno` to a value (probably `EAGAIN`) that reads `resource exhausted | resource temporarily unavailable` when looked up.

I tested calling `splice` with different `Length` values: `1024`, `8192`.

## Answer
        
I don't know Haskell, but "resource temporarily unavailable" is `EAGAIN`.

And it looks like [Haskell sets its sockets to non-blocking mode](http://hackage.haskell.org/packages/archive/network/2.3.0.11/doc/html/src/Network-Socket.html#line-427) by default. So if you try to read from one when there is no data, or try to write to one when its buffer is full, you will fail with `EAGAIN`.

Figure out how to change the sockets to blocking mode, and I bet you will solve your problem.

\[update\]

Alternatively, call `select` or `poll` before attempting to read or write the socket. But you still need to handle `EAGAIN`, because there are rare corner cases where Linux `select` will indicate a socket is ready when actually it isn't.
