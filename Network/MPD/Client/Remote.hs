{-|
Module      : Network.MPD.Client.Remote
Copyright   : (c) Joachim Fasting 2012
License     : MIT

Maintainer  : Joachim Fasting <joachim.fasting@gmail.com>
Stability   : unstable
Portability : unportable

Connection specification for MPD remotes.
-}

module Network.MPD.Client.Remote
    ( MpdRemote(..)
    , getRemoteSocket
    ) where

import           Control.Exception
import           Data.Default
import qualified Network.Socket as NS

------------------------------------------------------------------------

-- | MPD can be configured to communicate with clients over TCP or
-- over unix domain sockets.
--
-- Use 'getRemoteSocket' to acquire a socket.
--
-- Use "Data.Default.def" to get the default MPD remote
-- (passwordless localhost).
data MpdRemote
    = MpdHost
      { mpdHost :: NS.HostName
      , mpdPort :: Int
      , mpdPassword :: Maybe String
      }
    -- ^ Communicate with remote over a regular socket
    | MpdPipe String
    -- ^ Communicate over a unix domain socket
      deriving (Read, Show)

------------------------------------------------------------------------

instance Default MpdRemote where
    def = MpdHost
          { mpdHost = "localhost"
          , mpdPort = 6600
          , mpdPassword = Nothing
          }

------------------------------------------------------------------------

-- | Create a socket for communicating with MPD.
getRemoteSocket :: MpdRemote -> IO NS.Socket
getRemoteSocket r = do
    addr <- remoteAddr r
    sd <- NS.socket (NS.addrFamily addr)
                    (NS.addrSocketType addr)
                    (NS.addrProtocol addr)
    res <- tryIO $ NS.connect sd (NS.addrAddress addr)
    case res of
        Left e   -> NS.sClose sd >> throwIO e
        Right () -> return sd
    where
        -- if communicating over a unix domain socket, simply return a
        -- pre-filled addrinfo record
        remoteAddr (MpdPipe name) = do
             return $ NS.defaultHints
                      { NS.addrSocketType = NS.Stream
                      , NS.addrFamily     = NS.AF_UNIX
                      , NS.addrProtocol   = NS.defaultProtocol
                      , NS.addrAddress    = NS.SockAddrUnix name
                      }
        -- if communicating over tcp do a record lookup
        remoteAddr (MpdHost host port _) = do
             let hints = NS.defaultHints
                         { NS.addrSocketType = NS.Stream
                         }
             (addr:_) <- NS.getAddrInfo (Just hints)
                                        (Just host)
                                        (Just $ show port)
             return addr

------------------------------------------------------------------------

-- | A specialized version of 'try'.
tryIO :: IO a -> IO (Either SomeException a)
tryIO = try
{-# INLINE tryIO #-}
