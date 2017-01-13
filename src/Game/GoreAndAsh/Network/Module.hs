{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecursiveDo #-}
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
import Control.Monad.Trans.Control
import Control.Monad.Trans.Resource
import Data.Monoid
import Data.Proxy
import Data.Set (Set)
import Game.GoreAndAsh
import Game.GoreAndAsh.Logging
import Game.GoreAndAsh.Network.API
import Game.GoreAndAsh.Network.Backend
import Game.GoreAndAsh.Network.Options
import Game.GoreAndAsh.Network.State
import Network.Socket (withSocketsDo)

import qualified Data.Set as S

-- | Monad transformer of network core module.
--
-- [@t@] - FRP engine implementation, can be ignored almost everywhere.
--
-- [@b@] - Network backend the stack monad use;
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
newtype NetworkT t b m a = NetworkT { runNetworkT :: ReaderT (NetworkEnv t b) m a }
  deriving (Functor, Applicative, Monad, MonadReader (NetworkEnv t b), MonadFix
    , MonadIO, MonadThrow, MonadCatch, MonadMask, MonadSample t, MonadHold t)

instance MonadTrans (NetworkT t a) where
  lift = NetworkT . lift

instance MonadReflexCreateTrigger t m => MonadReflexCreateTrigger t (NetworkT t a m) where
  newEventWithTrigger = lift . newEventWithTrigger
  newFanEventWithTrigger initializer = lift $ newFanEventWithTrigger initializer

instance MonadSubscribeEvent t m => MonadSubscribeEvent t (NetworkT t a m) where
  subscribeEvent = lift . subscribeEvent

instance MonadAppHost t m => MonadAppHost t (NetworkT t a m) where
  getFireAsync = lift getFireAsync
  getRunAppHost = do
    runner <- NetworkT getRunAppHost
    return $ \m -> runner $ runNetworkT m
  performPostBuild_ = lift . performPostBuild_
  liftHostFrame = lift . liftHostFrame

instance MonadTransControl (NetworkT t b) where
  type StT (NetworkT t b) a = StT (ReaderT (NetworkEnv t b)) a
  liftWith = defaultLiftWith NetworkT runNetworkT
  restoreT = defaultRestoreT NetworkT

instance MonadBase b m => MonadBase b (NetworkT t a m) where
  liftBase = NetworkT . liftBase

instance (MonadBaseControl b m) => MonadBaseControl b (NetworkT t backend m) where
  type StM (NetworkT t backend m) a = ComposeSt (NetworkT t backend) m a
  liftBaseWith     = defaultLiftBaseWith
  restoreM         = defaultRestoreM

instance MonadResource m => MonadResource (NetworkT t a m) where
  liftResourceT = NetworkT . liftResourceT


instance ( MonadIO (HostFrame t)
         , MonadThrow (HostFrame t)
         , GameModule t m
         , MonadAppHost t m
         , MonadCatch m
         , LoggingMonad t m
         , MonadBaseControl IO m
         , HasNetworkBackend a
  ) => GameModule t (NetworkT t a m) where
  type ModuleOptions t (NetworkT t a m) = NetworkOptions (ModuleOptions t m) a

  runModule opts m = do
    s <- newNetworkEnv opts
    runModule (networkOptsNextOptions opts) (runReaderT (runNetworkT m') s)
    where
      m' = do
        handleNetworkServer
        handlePeersCollection =<< asks networkEnvPeers
        m

  withModule t _ =  withSocketsDo . withModule t (Proxy :: Proxy m)

-- | Update internal collection of peers
handlePeersCollection :: (MonadAppHost t m, NetworkServer t a m)
  => ExternalRef t (Set (Peer a)) -- ^ Collection of peers
  -> m ()
handlePeersCollection ref = do
  connE <- peerConnected
  dconnE <- peerDisconnected
  performEvent_ $ ffor connE $ \peer ->
    modifyExternalRef ref $ \ps -> (S.insert peer ps, ())
  performEvent_ $ ffor dconnE $ \peer ->
    modifyExternalRef ref $ \ps -> (S.delete peer ps, ())

-- | Watch after server peer creation/destruction and fill internal reference with
-- its current value
handleNetworkServer :: MonadAppHost t m => NetworkT t a m ()
handleNetworkServer = do
  NetworkEnv{..} <- ask
  let connE = networkEnvLocalConnected
      discE = networkEnvLocalDisconnected
  performEvent_ $ ffor connE $ \peer ->
    writeExternalRef networkEnvServer (Just peer)
  performEvent_ $ ffor discE $ const $
    writeExternalRef networkEnvServer Nothing

instance {-# OVERLAPPING #-} (
    MonadBaseControl IO m
  , MonadCatch m
  , MonadAppHost t m
  , LoggingMonad t m
  , HasNetworkBackend a
  ) => NetworkMonad t a (NetworkT t a m) where
  networkMessage = fmap (\(peer, ch, _, bs) -> (peer, ch, bs)) <$> asks networkEnvIncomingMessage
  {-# INLINE networkMessage #-}

  msgSendM peer chan mt msg = do
    NetworkEnv{..} <- ask
    let NetworkBackend{..} = networkEnvBackend
    let detailed = networkOptsDetailedLogging networkEnvOptions
    when detailed $ logMsgLnM LogInfo $ "Network: sending packet via channel "
       <> showl chan <> ", payload: " <> showl msg
    liftIO $ networkSendMessage peer chan mt msg
  {-# INLINE msgSendM #-}

  msgSend e = performAppHost $ ffor e $ \(peer, chan, mt, msg) -> msgSendM peer chan mt msg
  {-# INLINE msgSend #-}

  msgSendMany e = performAppHost $ ffor e $ \msgs -> forM_ msgs $
    \(peer, chan, mt, msg) -> msgSendM peer chan mt msg
  {-# INLINE msgSendMany #-}

  terminateBackend = do
    NetworkBackend{..} <- asks networkEnvBackend
    liftIO networkTerminate
  {-# INLINE terminateBackend #-}

  networkSomeError = asks networkEnvSomeError
  {-# INLINE networkSomeError #-}

  networkSendError = asks networkEnvSendError
  {-# INLINE networkSendError #-}

  networkConnectionError = asks networkEnvConnectionError
  {-# INLINE networkConnectionError #-}

instance {-# OVERLAPPING #-} (
    MonadBaseControl IO m
  , MonadCatch m
  , MonadAppHost t m
  , LoggingMonad t m
  , HasNetworkBackend a
  ) => NetworkServer t a (NetworkT t a m) where
  peerConnected = asks networkEnvRemoteConnected
  {-# INLINE peerConnected #-}

  peerDisconnected = asks networkEnvRemoteDisconnected
  {-# INLINE peerDisconnected #-}

  disconnectPeerM peer = do
    NetworkBackend{..} <- asks networkEnvBackend
    liftIO $ networkDisconnect peer
  {-# INLINE disconnectPeerM #-}

  disconnectPeer e = performAppHost $ fmap disconnectPeerM e
  {-# INLINE disconnectPeer #-}

  disconnectPeers e = performAppHost $ fmap (mapM_ disconnectPeerM) e
  {-# INLINE disconnectPeers #-}

  networkPeers = externalRefDynamic =<< asks networkEnvPeers
  {-# INLINE networkPeers #-}

instance {-# OVERLAPPING #-} (
    MonadBaseControl IO m
  , MonadCatch m
  , MonadAppHost t m
  , LoggingMonad t m
  , HasNetworkBackend a
  ) => NetworkClient t a (NetworkT t a m) where
  clientConnect e = do
    NetworkEnv{..} <- ask
    let NetworkBackend{..} = networkEnvBackend
    performEvent_ $ ffor e $ \(addr, opts) -> liftIO $ networkConnect addr opts
    return $ fmapMaybe id $ externalEvent networkEnvServer
  {-# INLINE clientConnect #-}

  serverPeer = externalRefDynamic =<< asks networkEnvServer
  {-# INLINE serverPeer #-}

  disconnectFromServerM = do
    NetworkEnv{..} <- ask
    modifyExternalRefM networkEnvServer $ \case
      Nothing -> return (Nothing, ())
      Just serv -> do
        liftIO $ networkDisconnect networkEnvBackend serv
        return (Nothing, ())
  {-# INLINE disconnectFromServerM #-}

  disconnectFromServer = performAppHost . fmap (const disconnectFromServerM)
  {-# INLINE disconnectFromServer #-}

  connected = asks (fcutMaybe . externalEvent . networkEnvServer)
  {-# INLINE connected #-}

  disconnected = asks (fkeepNothing . externalEvent . networkEnvServer)
  {-# INLINE disconnected #-}
