{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-|
Module      : Game.GoreAndAsh.Network
Description : Module that contains network low-level API for Gore&Ash
Copyright   : (c) Anton Gushcha, 2015-2016
License     : BSD3
Maintainer  : ncrashed@gmail.com
Stability   : experimental
Portability : POSIX

The core module contains API for basic networking for Gore&Ash. The network module is built
over Enet library, UDP transport with custom implementation of reliability. The API provides
connection handling and basic message handling (bytestring sending and receiving).

The module depends on following core modules:

* logging - "Game.GoreAndAsh.Logging"

So 'NetworkT' should be placed after 'LoggingT' in monad stack.

The module is NOT pure within first phase (see 'ModuleStack' docs), therefore currently only 'IO' end monad can handler the module.

Example of embedding:

@
-- | Application monad is monad stack build from given list of modules over base monad (IO)
type AppStack = ModuleStack [LoggingT, NetworkT, ... other modules ... ] IO
newtype AppState = AppState (ModuleState AppStack)
  deriving (Generic)

instance NFData AppState

-- | Wrapper around type family
newtype AppMonad a = AppMonad (AppStack a)
  deriving (Functor, Applicative, Monad, MonadFix, MonadIO, MonadThrow, MonadCatch, LoggingMonad, NetworkMonad, ... other modules monads ... )

instance GameModule AppMonad AppState where
  type ModuleState AppMonad = AppState
  runModule (AppMonad m) (AppState s) = do
    (a, s') <- runModule m s
    return (a, AppState s')
  newModuleState = AppState <$> newModuleState
  withModule _ = withModule (Proxy :: Proxy AppStack)
  cleanupModule (AppState s) = cleanupModule s

-- | Arrow that is build over the monad stack
type AppWire a b = GameWire AppMonad a b
@

-}
module Game.GoreAndAsh.Network(
  -- * Types
    Host
  , Peer
  , ChannelID
  , NetworkT
  -- ** Options
  , NetworkOptions
  , defaultNetworkOptions
  , networkChannelsCount
  , networkIncomingBandwidth
  , networkOutcomingBandwidth
  , networkDetailedLogging
  , networkNextOptions
  -- ** Errors
  , NetworkError(..)
  , renderNetworkError
  -- ** Messages
  , Message(..)
  , MessageType(..)
  , messageToPacket
  -- * Network API
  , NetworkMonad(..)
  , peerSend
  , peerChanSend
  , terminateNetwork
  , peerMessage
  , chanMessage
  , peerChanMessage
  -- ** Client API
  , NetworkClient(..)
  , ClientConnect(..)
  , whenConnected
  , whenConnectedWithDisconnect
  -- ** Server API
  , NetworkServer(..)
  , ServerListen(..)
  -- ** Collections
  , processPeers
  , HasDisconnect(..)
  , processPeersWithDisconnect
  ) where

-- imports for docs
import Game.GoreAndAsh.Core
import Game.GoreAndAsh.Logging
import Network.ENet.Bindings (ChannelID(..))

import Game.GoreAndAsh.Network.API as X
import Game.GoreAndAsh.Network.Error as X
import Game.GoreAndAsh.Network.Message as X
import Game.GoreAndAsh.Network.Module as X
import Game.GoreAndAsh.Network.Options as X
import Game.GoreAndAsh.Network.State as X