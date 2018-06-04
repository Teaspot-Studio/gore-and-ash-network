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

-}
module Game.GoreAndAsh.Network(
  -- * Types
    ChannelId(..)
  , RemoteAddress
  , MessageType(..)
  , HasNetworkBackend(..)
  , SendError(..)
  , NetworkT
  , withNetwork
  , runNetworkT
  -- ** Options
  , NetworkOptions
  , defaultNetworkOptions
  , networkOptsDetailedLogging
  , networkOptsBackendOptions
  -- ** Errors
  , NetworkError(..)
  , renderNetworkError
  -- * Network API
  , NetworkMonad(..)
  , peerSend
  , peerChanSend
  , peerSendMany
  , peerChanSendMany
  , terminateNetwork
  , peerMessage
  , chanMessage
  , peerChanMessage
  -- ** Client API
  , NetworkClient(..)
  , whenConnected
  , whenConnectedWithDisconnect
  -- ** Server API
  , NetworkServer(..)
  -- ** Collections
  , PeerAction(..)
  , peersCollection
  , peersCollectionWithDisconnect
  , processPeers
  , processPeersWithDisconnect
  ) where

import Game.GoreAndAsh.Network.API as X
import Game.GoreAndAsh.Network.Backend as X
import Game.GoreAndAsh.Network.Error as X
import Game.GoreAndAsh.Network.Module as X
import Game.GoreAndAsh.Network.Options as X
import Game.GoreAndAsh.Network.State as X
