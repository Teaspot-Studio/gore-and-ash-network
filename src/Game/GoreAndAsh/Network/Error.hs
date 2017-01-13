{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module      : Game.GoreAndAsh.Network.Error
Description : Network module possible errors
Copyright   : (c) Anton Gushcha, 2015-2016
License     : BSD3
Maintainer  : ncrashed@gmail.com
Stability   : experimental
Portability : POSIX
-}
module Game.GoreAndAsh.Network.Error(
    NetworkError(..)
  , renderNetworkError
  ) where

import Control.Exception
import Data.Monoid
import Data.Typeable
import GHC.Generics

import Game.GoreAndAsh.Logging
import Game.GoreAndAsh.Network.Backend

-- | Error that can be raised in network module and thrown to your code
data NetworkError a =
    NetworkBackendCreationFail (BackendCreateError a)
  deriving (Generic)

deriving instance HasNetworkBackend a => Show (NetworkError a)
instance (Typeable a, HasNetworkBackend a) => Exception (NetworkError a)

-- | Make human readable description of network error
renderNetworkError :: HasNetworkBackend a => NetworkError a -> LogStr
renderNetworkError ne = case ne of
  NetworkBackendCreationFail e -> "Failed to create backend for network module: " <> showl e
