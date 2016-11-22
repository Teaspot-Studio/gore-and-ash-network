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

import Data.Monoid
import Network.Socket (SockAddr)

import qualified Network.ENet.Bindings as B

import Game.GoreAndAsh.Logging
import Game.GoreAndAsh.Network.State
import Game.GoreAndAsh.Network.Message

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