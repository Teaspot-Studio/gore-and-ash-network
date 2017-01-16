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
  , networkOptsDetailedLogging
  , networkOptsBackendOptions
  , networkOptsNextOptions
  ) where

import Control.DeepSeq
import GHC.Generics

import Game.GoreAndAsh.Network.Backend

-- | Configuration of network module
data NetworkOptions s a = NetworkOptions {
    networkOptsDetailedLogging    :: !Bool -- ^ Should log everything or not
  , networkOptsBackendOptions     :: !(BackendOptions a) -- ^ Specific backend options
  , networkOptsNextOptions        :: !s -- ^ Options of underlying module
  }
  deriving (Generic)

instance (NFData s, NFData (BackendOptions a)) => NFData (NetworkOptions s a)

-- | Default values for client configuration of network module
defaultNetworkOptions :: BackendOptions a -- ^ Options for backend
  -> s -- ^ Options of underlying module
  -> NetworkOptions s a
defaultNetworkOptions opts s = NetworkOptions {
    networkOptsDetailedLogging = False
  , networkOptsBackendOptions = opts
  , networkOptsNextOptions = s
  }