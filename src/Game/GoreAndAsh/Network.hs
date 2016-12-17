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
  , networkPollTimeout
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