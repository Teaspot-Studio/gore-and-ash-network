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
    MessageEventPayload
  -- * State
  , NetworkEnv(..)
  , newNetworkEnv
  ) where

import Control.Monad
import Control.Monad.Catch
import Data.Set (Set)
import GHC.Generics (Generic)

import Game.GoreAndAsh
import Game.GoreAndAsh.Network.Error
import Game.GoreAndAsh.Network.Backend
import Game.GoreAndAsh.Network.Options

import qualified Data.ByteString as BS

-- | Holds peer the message come from, channel id and payload.
type MessageEventPayload a = (Peer a, ChannelId, MessageType, BS.ByteString)

-- | Inner state of network layer
--
-- [@t@] - FRP engine, you can safely ignore the parameter.
--
-- [@a@] - Backend for the network layer. For instance, 'TCPBackend'
data NetworkEnv t a = NetworkEnv {
  -- | Store connection options that were used to create the state.
  networkEnvOptions            :: !(NetworkOptions () a)
  -- | Backend that implements low-level network operations.
, networkEnvBackend            :: !(NetworkBackend a)
  -- | Stored information about connection to server. Used by client.
, networkEnvServer             :: !(ExternalRef t (Maybe (Peer a)))
  -- | Stored collection of connected peers.
, networkEnvPeers              :: !(ExternalRef t (Set (Peer a)))
  -- | Store current number of channels
, networkEnvMaxChannels        :: !(ExternalRef t Word)
  -- | Fires when local client has connected to server
, networkEnvLocalConnected     :: !(Event t (Peer a))
  -- | Fires when local client has disconnected from server
, networkEnvLocalDisconnected  :: !(Event t ())
  -- | Fires when a remote client has connected to server
, networkEnvRemoteConnected    :: !(Event t (Peer a))
  -- | Fires when a remote client has disconnected from server
, networkEnvRemoteDisconnected :: !(Event t (Peer a))
  -- | Event about incomming network message. Holds peer the message come from,
  -- channel id and payload.
, networkEnvIncomingMessage    :: !(Event t (MessageEventPayload a))
  -- | Event about async error came from backend layer
, networkEnvSomeError          :: !(Event t (BackendEventError a))
  -- | Event about something bad happened when tried to send a message
, networkEnvSendError          :: !(Event t (SendError a))
  -- | Event about connection error
, networkEnvConnectionError    :: !(Event t (BackendConnectError a, RemoteAddress))
} deriving (Generic)

-- | Creates initial state and backend
newNetworkEnv :: forall t m a s . (MonadAppHost t m, MonadThrow m, HasNetworkBackend a)
  => NetworkOptions s a -- ^ Initialisation options
  -> m (NetworkEnv t a)
newNetworkEnv opts = do
  serv <- newExternalRef Nothing
  peers <- newExternalRef mempty
  maxchans <- newExternalRef 0
  (localConnectedE, localConnectedTrigger) <- newExternalEvent
  (localDisconnectedE, localDisconnectedTrigger) <- newExternalEvent
  (remoteConnectedE, remoteConnectedTrigger) <- newExternalEvent
  (remoteDisconnectedE, remoteDisconnectedTrigger) <- newExternalEvent
  (incomingMessageE, incomingMessageTrigger) <- newExternalEvent
  (someErrorE, someErrorTrigger) <- newExternalEvent
  (sendErrorE, sendErrorTrigger) <- newExternalEvent
  (connErrorE, connErrorTrigger) <- newExternalEvent
  let
    backendContext = NetworkBackendContext {
        networkBackendOptions           = networkOptsBackendOptions opts
      , networkTriggerLocalConnected    = void . localConnectedTrigger
      , networkTriggerLocalDisonnected  = void $ localDisconnectedTrigger ()
      , networkTriggerRemoteConnected   = void . remoteConnectedTrigger
      , networkTriggerRemoteDisonnected = void . remoteDisconnectedTrigger
      , networkTriggerIncomingMessage   = \p c t b -> void . incomingMessageTrigger $ (p, c, t, b)
      , networkTriggerSomeError         = void . someErrorTrigger
      , networkTriggerSendError         = void . sendErrorTrigger
      , networkTriggerConnectionError   = void . connErrorTrigger
      }
  mbackend <- createNetworkBackend backendContext
  case mbackend of
    Left er -> throwM (NetworkBackendCreationFail er :: NetworkError a)
    Right backend -> return NetworkEnv {
        networkEnvOptions             = opts { networkOptsNextOptions = () }
      , networkEnvBackend             = backend
      , networkEnvServer              = serv
      , networkEnvPeers               = peers
      , networkEnvMaxChannels         = maxchans
      , networkEnvLocalConnected      = localConnectedE
      , networkEnvLocalDisconnected   = localDisconnectedE
      , networkEnvRemoteConnected     = remoteConnectedE
      , networkEnvRemoteDisconnected  = remoteDisconnectedE
      , networkEnvIncomingMessage     = incomingMessageE
      , networkEnvSomeError           = someErrorE
      , networkEnvSendError           = sendErrorE
      , networkEnvConnectionError     = connErrorE
      }