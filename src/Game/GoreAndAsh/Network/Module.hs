{-# LANGUAGE LambdaCase #-}
--{-# OPTIONS_GHC -fno-warn-orphans #-}
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
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Trans.Resource
import Data.Monoid
import Data.Proxy
import Game.GoreAndAsh
import Game.GoreAndAsh.Network.Error
import Game.GoreAndAsh.Network.Options
import Game.GoreAndAsh.Network.State
import Network.ENet
import Network.ENet.Host
import Network.ENet.Packet (peek)

import qualified Network.ENet.Bindings as B

-- | Monad transformer of network core module.
--
-- [@t@] - FRP engine implementation, can be ignored almost everywhere.
--
-- [@m@] - Next monad in modules monad stack;
--
-- [@a@] - Type of result value;
--
-- How to embed module:
--
-- @
-- newtype AppMonad t a = AppMonad (LoggingT (NetworkT t (GameMonad t)) a)
--   deriving (Functor, Applicative, Monad, MonadFix, MonadIO, LoggingMonad, NetworkMonad)
-- @
newtype NetworkT t m a = NetworkT { runNetworkT :: ReaderT (NetworkState t) m a }
  deriving (Functor, Applicative, Monad, MonadReader (NetworkState t), MonadFix
    , MonadIO, MonadThrow, MonadCatch, MonadMask, MonadSample t, MonadHold t)

instance MonadTrans (NetworkT t) where
  lift = NetworkT . lift

instance MonadCatch m => MonadError NetworkError (NetworkT t m) where
  throwError = throwM
  catchError = catch

instance MonadReflexCreateTrigger t m => MonadReflexCreateTrigger t (NetworkT t m) where
  newEventWithTrigger = lift . newEventWithTrigger
  newFanEventWithTrigger initializer = lift $ newFanEventWithTrigger initializer

instance MonadSubscribeEvent t m => MonadSubscribeEvent t (NetworkT t m) where
  subscribeEvent = lift . subscribeEvent

instance MonadAppHost t m => MonadAppHost t (NetworkT t m) where
  getFireAsync = lift getFireAsync
  getRunAppHost = do
    runner <- NetworkT getRunAppHost
    return $ \m -> runner $ runNetworkT m
  performPostBuild_ = lift . performPostBuild_
  liftHostFrame = lift . liftHostFrame

instance MonadBase IO m => MonadBase IO (NetworkT t m) where
  liftBase = NetworkT . liftBase

instance MonadResource m => MonadResource (NetworkT t m) where
  liftResourceT = NetworkT . liftResourceT

-- | Action that fires event about incoming message
type MessageEventFire = MessageEventPayload -> IO Bool
-- | Action that fires event about peer connection
type ConnectPeerFire = Peer -> IO Bool
-- | Action that fires event about peer disconnection
type DisconnectPeerFire = Peer -> IO Bool

instance (MonadIO (HostFrame t), GameModule t m) => GameModule t (NetworkT t m) where
  type ModuleOptions t (NetworkT t m) = NetworkOptions (ModuleOptions t m)

  runModule opts m = do
    (messageE, messageFire) <- newExternalEvent
    (connPeerE, fireConnPeer) <- newExternalEvent
    (disconnPeerE, fireDisconnPeer) <- newExternalEvent
    s <- newNetworkState opts messageE connPeerE disconnPeerE fireDisconnPeer
    a <- runModule (networkNextOptions opts) (runReaderT (runNetworkT m) s)
    processNetEvents s messageFire fireConnPeer fireDisconnPeer
    return a

  withModule t _ = withENetDo . withModule t (Proxy :: Proxy m)

-- | Poll all events from ENet, does nothing when system doesn't have any initiated host object
processNetEvents :: forall t m . MonadAppHost t m
  => NetworkState t -- ^ Context that holds reference with host
  -> MessageEventFire -- ^ Action that fires event about incoming message
  -> ConnectPeerFire  -- ^ Action that fires event about connected peer
  -> DisconnectPeerFire -- ^ Action that fires event about disconnected peer
  -> m ()
processNetEvents st msgFire fireConnPeer fireDisconnPeer = do
  _ <- holdAppHost (return ()) $ ffor (externalEvent $ networkStateHost st) $ \case
    Nothing -> return ()
    Just host -> handleEvents host
  return ()
  where
    handleEvents :: Host -> m ()
    handleEvents host = untilNothing (service host 0) handleEvent

    handleEvent :: B.Event -> m ()
    handleEvent (B.Event et peer ch edata packetPtr) = case et of
      B.None -> detailLog "Network: Event none"
      B.Connect -> do
        detailLog "Network: Peer connected"
        _ <- liftIO $ fireConnPeer peer
        return ()
      B.Disconnect -> do
        detailLog $ "Network: Peer disconnected, code " <> show edata
        _ <- liftIO $ fireDisconnPeer peer
        return ()
      B.Receive -> do
        (Packet !fs !bs) <- liftIO $ peek packetPtr
        detailLog $ "Network: Received message at channel " <> show ch <> ": "
          <> show fs <> ", payload: " <> show bs
        _ <- liftIO $ msgFire (peer, ch, bs)
        return ()

    detailLog = when (networkDetailedLogging $ networkStateOptions st) . liftIO . putStrLn

    untilNothing :: IO (Maybe a) -> (a -> m ()) -> m ()
    untilNothing f h = do
      ma <- liftIO f
      case ma of
        Nothing -> return ()
        Just a -> do
          h a
          untilNothing f h