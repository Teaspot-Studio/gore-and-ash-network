{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module      : Game.GoreAndAsh.Network.Options
Description : Module creation options
Copyright   : (c) Anton Gushcha, 2015-2016
License     : BSD3
Maintainer  : ncrashed@gmail.com
Stability   : experimental
Portability : POSIX
-}
module Game.GoreAndAsh.Network.Options(
    NetworkOptions
  , defaultNetworkOptions
  , networkChannelsCount
  , networkIncomingBandwidth
  , networkOutcomingBandwidth
  , networkDetailedLogging
  , networkNextOptions
  ) where

import Control.DeepSeq
import Data.Word
import GHC.Generics

-- | Configuration of network module
data NetworkOptions s = NetworkOptions {
    networkChannelsCount      :: !Word -- ^ Number of channels to open
  , networkIncomingBandwidth  :: !Word32 -- ^ Incoming max bandwidth
  , networkOutcomingBandwidth :: !Word32 -- ^ Outcoming max bandwidth
  , networkDetailedLogging    :: !Bool -- ^ Should log everything or not
  , networkNextOptions        :: !s -- ^ Options of underlying module
  }
  deriving (Generic)

instance NFData s => NFData (NetworkOptions s)

-- | Default values for client configuration of network module
--
-- @
-- NetworkOptions {
-- , networkChannelsCount = 2
-- , networkIncomingBandwidth = 0
-- , networkOutcomingBandwidth = 0
-- , networkDetailedLogging = False
-- , networkNextOptions = s
-- }
-- @
defaultNetworkOptions :: s -- ^ Options of underlying module
  -> NetworkOptions s
defaultNetworkOptions s = NetworkOptions {
    networkChannelsCount = 2
  , networkIncomingBandwidth = 0
  , networkOutcomingBandwidth = 0
  , networkDetailedLogging = False
  , networkNextOptions = s
  }