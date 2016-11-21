{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module      : Game.GoreAndAsh.Network.Module
Description : Monad transformer and core module instance
Copyright   : (c) Anton Gushcha, 2015-2016
License     : BSD3
Maintainer  : ncrashed@gmail.com
Stability   : experimental
Portability : POSIX

The module contains declaration of module monad transformer and instance of 'GameModule'.
-}
module Game.GoreAndAsh.Network.Module(
    NetworkT(..)
  ) where

import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Error.Class
import Control.Monad.Extra (whenJust)
import Control.Monad.Fix
import Control.Monad.State.Strict
import Control.Monad.Trans.Resource
import Data.Hashable
import Data.Proxy
import Game.GoreAndAsh
import Game.GoreAndAsh.Network.State
import Network.ENet
import Network.ENet.Host
import Network.ENet.Packet (peek)
import Network.ENet.Peer
import qualified Data.Foldable as F
import qualified Data.HashMap.Strict as H
import qualified Data.Sequence as S
import qualified Network.ENet.Bindings as B

-- | Monad transformer of network core module.
--
-- [@m@] - Next monad in modules monad stack;
--
-- [@a@] - Type of result value;
--
-- How to embed module:
--
-- @
-- newtype AppMonad t a = AppMonad (LoggingT (NetworkT (GameMonad t)) a)
--   deriving (Functor, Applicative, Monad, MonadFix, MonadIO, LoggingMonad, NetworkMonad)
-- @
newtype NetworkT m a = NetworkT { runNetworkT :: StateT NetworkState m a }
  deriving (Functor, Applicative, Monad, MonadState NetworkState, MonadFix
    , MonadTrans, MonadIO, MonadThrow, MonadCatch, MonadMask, MonadError e
    , MonadSample t, MonadHold t)

instance MonadReflexCreateTrigger t m => MonadReflexCreateTrigger t (NetworkT m) where
  newEventWithTrigger = lift . newEventWithTrigger
  newFanEventWithTrigger initializer = lift $ newFanEventWithTrigger initializer

instance MonadAppHost t m => MonadAppHost t (NetworkT m) where
  getFireAsync = lift getFireAsync
  getRunAppHost = do
    runner <- NetworkT getRunAppHost
    return $ \m -> runner $ runNetworkT m
  performPostBuild_ = lift . performPostBuild_
  liftHostFrame = lift . liftHostFrame

instance MonadBase IO m => MonadBase IO (NetworkT s m) where
  liftBase = NetworkT . liftBase

instance MonadResource m => MonadResource (NetworkT s m) where
  liftResourceT = NetworkT . liftResourceT

instance GameModule t m => GameModule t (NetworkT m) where

  runModule (NetworkT m) s = do
    ((a, s'), nextState) <- runModule (runStateT m s) (networkNextState s)
    s'' <- processEvents <=< clearMessages <=< moveDisconnected <=< moveConnected $ s'
    return (a, s'' {
        networkNextState = nextState
      })
    where
      processEvents s' = case networkHost s' of
        Nothing -> return s'
        Just h -> processNetEvents s' h

      moveConnected s' = return $ s' {
          networkPeers = networkPeers s' S.>< networkConnectedPeers s'
        , networkConnectedPeers = S.empty
        }

      moveDisconnected s' = return $ s' {
          networkPeers = remAllFromSeq (networkDisconnectedPeers s') (networkPeers s')
        , networkDisconnectedPeers = S.empty
        }

      clearMessages s' = return $ s' {
          networkMessages = H.empty
        }

  withModule t _ = withENetDo . withModule t (Proxy :: Proxy m)

-- | Safe cleanup of network system
cleanupModule :: NetworkState -> IO ()
cleanupModule NetworkState{..} = do
  forM_ networkPeers $ \p -> disconnectNow p 0
  forM_ networkConnectedPeers $ \p -> disconnectNow p 0
  whenJust networkHost destroy

-- | Deletes all elements from second sequence that are in first sequence O(n^2)
remAllFromSeq :: (Eq k, Hashable k) => S.Seq k -> S.Seq k -> S.Seq k
remAllFromSeq s m = F.foldl' (\acc a -> S.filter (/= a) acc) m s

-- | Poll all events from ENet
processNetEvents :: MonadIO m => NetworkState -> Host -> m NetworkState
processNetEvents nst hst = liftIO $ untilNothing nst (service hst 0) handleEvent
  where
    untilNothing acc f h = do
      ma <- f
      case ma of
        Nothing -> return acc
        Just a -> do
          acc' <- h acc a
          untilNothing acc' f h

    handleEvent s@NetworkState{..} (B.Event et peer ch edata packetPtr) = case et of
      B.None -> do
        when networkDetailedLogging $ putStrLn "Network: Event none"
        return s
      B.Connect -> do
        when networkDetailedLogging $ putStrLn "Network: Peer connected"
        return $ s {
            networkConnectedPeers = networkConnectedPeers S.|> peer
          }
      B.Disconnect -> do
        when networkDetailedLogging $ putStrLn $ "Network: Peer disconnected, code " ++ show edata
        return $ s {
            networkDisconnectedPeers = networkDisconnectedPeers S.|> peer
          }
      B.Receive -> do
        (Packet !fs !bs) <- peek packetPtr
        when networkDetailedLogging $ putStrLn $ "Network: Received message at channel " ++ show ch ++ ": "
          ++ show fs ++ ", payload: " ++ show bs
        return $ s {
            networkMessages = case H.lookup (peer, ch) networkMessages of
              Nothing -> H.insert (peer, ch) (S.singleton bs) networkMessages
              Just msgs -> H.insert (peer, ch) (msgs S.|> bs) networkMessages
          }