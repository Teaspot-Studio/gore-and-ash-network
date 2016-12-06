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
  , MessageEventPayload
  -- * State
  , NetworkEnv
  , networkEnvHost
  , networkEnvServer
  , networkEnvPeerConnect
  , newtorkStatePeerDisconnect
  , networkEnvPeerDisconnectFire
  , networkEnvOptions
  , networkEnvMessageEvent
  , networkEnvMaxChannels
  , newNetworkEnv
  ) where

import Control.DeepSeq
import Foreign
import GHC.Generics (Generic)

import Game.GoreAndAsh
import Game.GoreAndAsh.Network.Options

import qualified Data.ByteString as BS
import qualified Network.ENet.Bindings as B

-- | Local endpoint
type Host = Ptr B.Host
-- | Remote endpoint
type Peer = Ptr B.Peer

-- | Holds peer the message come from, channel id and payload.
type MessageEventPayload = (Peer, B.ChannelID, BS.ByteString)

-- | Inner state of network layer
--
-- [@t@] - FRP engine, you can safely ignore the parameter.
data NetworkEnv t = NetworkEnv {
  -- | There a ENet host object is stored once the local server started to listen
  -- or local client is connected to remote server.
  --
  -- Note that host is create both for clients and servers.
  networkEnvHost :: !(ExternalRef t (Maybe Host))
  -- | Stored information about connection to server. Used by client.
, networkEnvServer :: !(ExternalRef t (Maybe Peer))
  -- | Fires when a new peer is connected
, networkEnvPeerConnect :: !(Event t Peer)
  -- | Fires when a peer is disconnected
, newtorkStatePeerDisconnect :: !(Event t Peer)
  -- | Action that fires event about peer disconnection
, networkEnvPeerDisconnectFire :: !(Peer -> IO Bool)
  -- | Store connection options that were used to create the state.
, networkEnvOptions :: !(NetworkOptions ())
  -- | Event about incomming network message. Holds peer the message come from,
  -- channel id and payload.
, networkEnvMessageEvent :: !(Event t MessageEventPayload)
  -- | Store current number of channels
, networkEnvMaxChannels :: !(ExternalRef t Word)
} deriving (Generic)

instance NFData (NetworkEnv t) where
  rnf NetworkEnv{..} = networkEnvHost `seq`
    networkEnvPeerConnect `seq`
    newtorkStatePeerDisconnect `seq`
    networkEnvPeerDisconnectFire `seq`
    networkEnvServer `seq`
    networkEnvOptions `deepseq`
    networkEnvMessageEvent `seq`
    networkEnvMaxChannels `seq`
    ()

-- | Creates initial state
newNetworkEnv :: MonadAppHost t m
  => NetworkOptions s -- ^ Initialisation options
  -> Event t MessageEventPayload -- ^ Event that fires when a network message arrives
  -> Event t Peer -- ^ Connection event
  -> Event t Peer -- ^ Disconnection event
  -> (Peer -> IO Bool) -- | Action that fires event about peer disconnection
  -> m (NetworkEnv t)
newNetworkEnv opts msgE peerConn peerDisconn fireDisconnect = do
  host <- newExternalRef Nothing
  serv <- newExternalRef Nothing
  maxchans <- newExternalRef 0
  return NetworkEnv {
      networkEnvHost = host
    , networkEnvServer = serv
    , networkEnvPeerConnect = peerConn
    , newtorkStatePeerDisconnect = peerDisconn
    , networkEnvPeerDisconnectFire = fireDisconnect
    , networkEnvOptions = opts { networkNextOptions = () }
    , networkEnvMessageEvent = msgE
    , networkEnvMaxChannels = maxchans
    }