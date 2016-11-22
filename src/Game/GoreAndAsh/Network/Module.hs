{-# LANGUAGE LambdaCase #-}
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
  -- * Helpers
  , networkBind
  , networkConnect
  , disconnectAll
  ) where

import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Trans.Resource
import Data.Monoid
import Data.Proxy
import Data.Word
import Foreign (nullPtr)
import Game.GoreAndAsh
import Game.GoreAndAsh.Logging
import Game.GoreAndAsh.Network.Error
import Game.GoreAndAsh.Network.Options
import Game.GoreAndAsh.Network.State
import Network.ENet
import Network.ENet.Host
import Network.ENet.Packet (peek)
import Network.ENet.Peer
import Network.Socket (SockAddr)

import qualified Data.Sequence as S
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
    , MonadTrans, MonadIO, MonadThrow, MonadCatch, MonadMask
    , MonadSample t, MonadHold t)

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

instance (MonadIO (HostFrame t), GameModule t m) => GameModule t (NetworkT t m) where
  type ModuleOptions t (NetworkT t m) = NetworkOptions (ModuleOptions t m)

  runModule opts m = do
    (messageE, messageFire) <- newExternalEvent
    s <- newNetworkState opts messageE
    a <- runModule (networkNextOptions opts) (runReaderT (runNetworkT m) s)
    processNetEvents s messageFire
    return a

  withModule t _ = withENetDo . withModule t (Proxy :: Proxy m)

-- | Terminate all connections and destroy host object
disconnectAll :: MonadAppHost t m => NetworkState t -> m ()
disconnectAll NetworkState{..} = do
  modifyExternalRefM networkPeers $ \peers -> do
    forM_ peers $ \p -> liftIO $ disconnectNow p 0
    return (mempty, ())

  modifyExternalRefM networkHost $ \case
    Nothing -> return (Nothing, ())
    Just host -> do
      liftIO $ destroy host
      return (Nothing, ())

-- | Poll all events from ENet, does nothing when system doesn't have any initiated host object
processNetEvents :: forall t m . MonadAppHost t m
  => NetworkState t -- ^ Context that holds reference with host
  -> MessageEventFire -- ^ Action that fires event about incoming message
  -> m ()
processNetEvents NetworkState{..} msgFire = do
  _ <- holdAppHost (return ()) $ ffor (externalEvent networkHost) $ \case
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
        modifyExternalRef networkPeers $ \ps -> (ps S.|> peer, ())
      B.Disconnect -> do
        detailLog $ "Network: Peer disconnected, code " <> show edata
        modifyExternalRef networkPeers $ \ps -> (S.filter (/= peer) ps, ())
      B.Receive -> do
        (Packet !fs !bs) <- liftIO $ peek packetPtr
        detailLog $ "Network: Received message at channel " <> show ch <> ": "
          <> show fs <> ", payload: " <> show bs
        _ <- liftIO $ msgFire (peer, ch, bs)
        return ()

    detailLog = when (networkDetailedLogging networkOptions) . liftIO . putStrLn

    untilNothing :: IO (Maybe a) -> (a -> m ()) -> m ()
    untilNothing f h = do
      ma <- liftIO f
      case ma of
        Nothing -> return ()
        Just a -> do
          h a
          untilNothing f h

-- | Initialise network system and create host
networkBind :: (LoggingMonad t m, MonadError NetworkError m)
  => Maybe SockAddr -- ^ Address to listen, Nothing is client
  -> Word -- ^ Maximum count of connections
  -> Word -- ^ Number of channels to open
  -> Word32 -- ^ Incoming max bandwidth
  -> Word32 -- ^ Outcoming max bandwidth
  -> Bool -- ^ Detailed logging
  -> m Host
networkBind addr conCount chanCount inBandth outBandth detailed = do
  phost <- liftIO $ create addr (fromIntegral conCount) (fromIntegral chanCount) inBandth outBandth
  if phost == nullPtr
    then throwError NetworkInitFail
    else do
      when detailed $ logMsgMLn LogInfo $ case addr of
        Nothing -> "Network: client network system initalized"
        Just a -> "Network: binded to " <> showl a
      return phost

-- | Connect to remote server
networkConnect :: (LoggingMonad t m, MonadError NetworkError m)
  => Host -- ^ Initialised host (local network system)
  -> SockAddr -- ^ Address of host
  -> Word -- ^ Count of channels to open
  -> Word32 -- ^ Additional data (0 default)
  -> Bool -- ^ Detailed logging (log debug info)
  -> m Peer
networkConnect host addr chanCount datum detailed = do
  peer <- liftIO $ connect host addr (fromIntegral chanCount) datum
  unless (peer == nullPtr) $ throwError $ NetworkConnectFail addr
  when detailed $ logMsgMLn LogInfo $ "Network: connected to " <> showl addr
  return peer
