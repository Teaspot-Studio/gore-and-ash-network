{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module      : Game.GoreAndAsh.Network.State
Description : Internal state of core module
Copyright   : (c) Anton Gushcha, 2015-2016
License     : BSD3
Maintainer  : ncrashed@gmail.com
Stability   : experimental
Portability : POSIX
-}
module Game.GoreAndAsh.Network.State(
  -- * Types
    Host
  , Peer
  , B.ChannelID(..)
  -- * Options
  , NetworkOptions(..)
  , defaultClientOptions
  , defaultServerOptions
  -- * State
  , NetworkState(..)
  , newNetworkState
  -- * Errors
  , NetworkError(..)
  , renderNetworkError
  ) where

import Control.DeepSeq
import Control.Monad.Except
import Data.Hashable
import Data.Monoid
import Foreign
import GHC.Generics (Generic)
import Network.ENet.Host
import Network.Socket (SockAddr)

import Game.GoreAndAsh
import Game.GoreAndAsh.Logging
import Game.GoreAndAsh.Network.Message

import qualified Data.Sequence as S
import qualified Network.ENet.Bindings as B

-- | Local endpoint
type Host = Ptr B.Host
-- | Remote endpoint
type Peer = Ptr B.Peer

instance Hashable Peer where
  hashWithSalt s ptr = hashWithSalt s i
    where
      i :: Int
      i = fromIntegral $ ptrToIntPtr ptr

instance Hashable B.ChannelID where
  hashWithSalt s (B.ChannelID i) = hashWithSalt s i

-- | Configuration of network module
data NetworkOptions =
  -- | Server configuration
    NetworkServerOptions {
      networkListenAddr         :: !SockAddr -- ^ Address to listen 0.0.0.0 to listen everything
    , networkMaxConnections     :: !Word -- ^ Maximum count of connections
    , networkChannelsCount      :: !Word -- ^ Number of channels to open
    , networkIncomingBandwidth  :: !Word32 -- ^ Incoming max bandwidth
    , networkOutcomingBandwidth :: !Word32 -- ^ Outcoming max bandwidth
    , networkDetailedLogging    :: !Bool -- ^ Should log everything or not
    }
  -- | Client configuration
  | NetworkClientOptions {
      networkConnectAddr        :: !SockAddr -- ^ Address of remote server to connect to
    , networkChannelsCount      :: !Word -- ^ Number of channels to open
    , networkIncomingBandwidth  :: !Word32 -- ^ Incoming max bandwidth
    , networkOutcomingBandwidth :: !Word32 -- ^ Outcoming max bandwidth
    , networkDetailedLogging    :: !Bool -- ^ Should log everything or not
    }
  deriving (Generic)

instance NFData SockAddr where
  rnf addr = addr `seq` ()

instance NFData NetworkOptions

-- | Default values for client configuration of network module
--
-- @
-- NetworkClientOptions {
--   networkConnectAddr = addr
-- , networkChannelsCount = 2
-- , networkIncomingBandwidth = 0
-- , networkOutcomingBandwidth = 0
-- , networkDetailedLogging = False
-- }
-- @
defaultClientOptions :: SockAddr -- ^ Connection address of remote server
  -> NetworkOptions
defaultClientOptions addr = NetworkClientOptions {
    networkConnectAddr = addr
  , networkChannelsCount = 2
  , networkIncomingBandwidth = 0
  , networkOutcomingBandwidth = 0
  , networkDetailedLogging = False
  }

-- | Default values for server configuration of network module
--
-- @
-- NetworkServerOptions {
--   networkListenAddr = addr
-- , networkMaxConnections = 100
-- , networkIncomingBandwidth = 0
-- , networkOutcomingBandwidth = 0
-- , networkDetailedLogging = False
-- }
-- @
defaultServerOptions :: SockAddr -- ^ Listen address, can be '0.0.0.0'
  -> NetworkOptions
defaultServerOptions addr = NetworkServerOptions {
    networkListenAddr = addr
  , networkMaxConnections = 100
  , networkChannelsCount = 2
  , networkIncomingBandwidth = 0
  , networkOutcomingBandwidth = 0
  , networkDetailedLogging = False
  }

-- | Inner state of network layer
--
-- [@s@] - State of next module, the states are chained via nesting.
data NetworkState t = NetworkState {
  networkHost :: !Host -- ^ Note that host is create both for clients and servers
, networkPeers :: !(ExternalRef t (S.Seq Peer))
, networkOptions :: !NetworkOptions
} deriving (Generic)

instance NFData (NetworkState t)

instance NFData B.ChannelID where
  rnf (B.ChannelID i) = i `seq` ()

-- | Creates initial state, initialise local host, starts server either connects
-- to remote server.
newNetworkState :: (MonadAppHost t m, MonadError NetworkError m, LoggingMonad t m)
  => NetworkOptions
  -> m (NetworkState t)
newNetworkState opts = do
  host <- case opts of
    NetworkServerOptions {..} -> networkBind
      (Just networkListenAddr)
      networkMaxConnections
      networkChannelsCount
      networkIncomingBandwidth
      networkOutcomingBandwidth
      networkDetailedLogging
    NetworkClientOptions {..} -> do
      host <- networkBind
        Nothing
        1
        networkChannelsCount
        networkIncomingBandwidth
        networkOutcomingBandwidth
        networkDetailedLogging
      _ <- networkConnect
        host
        networkConnectAddr
        networkChannelsCount
        0 -- default additional data
        networkDetailedLogging
      return host
  peers <- newExternalRef mempty
  return NetworkState {
      networkHost = host
    , networkPeers = peers
    , networkOptions = opts
    }

-- | Error that can be raised in network module
data NetworkError =
    NetworkInitFail -- ^ Failed to initialise network host
  | NetworkConnectFail SockAddr -- ^ Failed to connect to remote server
  | NetworkSendFail Peer B.ChannelID Message -- ^ Failed to send message

-- | Make human readable description of network error
renderNetworkError :: NetworkError -> LogStr
renderNetworkError e = case e of
  NetworkInitFail -> "Failed to initialise network module (bind failed)"
  NetworkConnectFail addr -> "Failed to connect to addr '" <> showl addr <> "'"
  NetworkSendFail _ ch msg -> "Failed to send message over channel '"
    <> showl ch <> "' with payload '"
    <> showl msg <> "'"

-- | Initialise network system and create host
networkBind :: (LoggingMonad t m, MonadError NetworkError m)
  => Maybe SockAddr -- ^ Address to listen, Nothing is client
  -> Word -- ^ Maximum count of connections
  -> Word -- ^ Number of channels to open
  -> Word32 -- ^ Incoming max bandwidth
  -> Word32 -- ^ Outcoming max bandwidth
  -> Bool -- ^ Detailed logging
  -> m Host
networkBind addr conCount chanCount inBandth outBandth detailed = do
  phost <- liftIO $ create addr (fromIntegral conCount) (fromIntegral chanCount) inBandth outBandth
  if phost == nullPtr
    then throwError NetworkInitFail
    else do
      when detailed $ logMsgMLn LogInfo $ case addr of
        Nothing -> "Network: client network system initalized"
        Just a -> "Network: binded to " <> showl a
      return phost

-- | Connect to remote server
networkConnect :: (LoggingMonad t m, MonadError NetworkError m)
  => Host -- ^ Initialised host (local network system)
  -> SockAddr -- ^ Address of host
  -> Word -- ^ Count of channels to open
  -> Word32 -- ^ Additional data (0 default)
  -> Bool -- ^ Detailed logging (log debug info)
  -> m Peer
networkConnect host addr chanCount datum detailed = do
  peer <- liftIO $ connect host addr (fromIntegral chanCount) datum
  unless (peer == nullPtr) $ throwError $ NetworkConnectFail addr
  when detailed $ logMsgMLn LogInfo $ "Network: connected to " <> showl addr
  return peer
