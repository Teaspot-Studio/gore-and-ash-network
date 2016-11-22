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
  -- * State
  , NetworkState(..)
  , newNetworkState
  ) where

import Control.DeepSeq
import Foreign
import GHC.Generics (Generic)

import Game.GoreAndAsh
import Game.GoreAndAsh.Network.Options

import qualified Data.Sequence as S
import qualified Network.ENet.Bindings as B

-- | Local endpoint
type Host = Ptr B.Host
-- | Remote endpoint
type Peer = Ptr B.Peer

-- | Inner state of network layer
--
-- [@t@] - FRP engine, you can safely ignore the parameter.
data NetworkState t = NetworkState {
  -- | There a ENet host object is stored once the local server started to listen
  -- or local client is connected to remote server.
  --
  -- Note that host is create both for clients and servers.
  networkHost :: !(ExternalRef t (Maybe Host))
  -- | Stored information of connected peers for the local server.
, networkPeers :: !(ExternalRef t (S.Seq Peer))
  -- | Store connection options that were used to create the state.
, networkOptions :: !(NetworkOptions ())
} deriving (Generic)

instance NFData (NetworkState t)

-- | Creates initial state
newNetworkState :: MonadAppHost t m
  => NetworkOptions s -- ^ Initialisation options
  -> m (NetworkState t)
newNetworkState opts = do
  host <- newExternalRef Nothing
  peers <- newExternalRef mempty
  return NetworkState {
      networkHost = host
    , networkPeers = peers
    , networkOptions = opts { networkNextOptions = () }
    }