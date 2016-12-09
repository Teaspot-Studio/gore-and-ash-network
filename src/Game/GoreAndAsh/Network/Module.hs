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

import Control.Concurrent
import Control.Exception.Base (IOException)
import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Control.Monad.Trans.Resource
import Data.Align
import Data.Monoid
import Data.Proxy
import Data.These
import Data.Word
import Foreign hiding (peek)
import Game.GoreAndAsh
import Game.GoreAndAsh.Logging
import Game.GoreAndAsh.Network.API
import Game.GoreAndAsh.Network.Error
import Game.GoreAndAsh.Network.Message
import Game.GoreAndAsh.Network.Options
import Game.GoreAndAsh.Network.State
import Network.ENet
import Network.ENet.Host
import Network.ENet.Packet (peek)
import Network.Socket (SockAddr)

import qualified Data.Sequence as S
import qualified Network.ENet.Bindings as B
import qualified Network.ENet.Packet as P
import qualified Network.ENet.Peer as P

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
newtype NetworkT t m a = NetworkT { runNetworkT :: ReaderT (NetworkEnv t) m a }
  deriving (Functor, Applicative, Monad, MonadReader (NetworkEnv t), MonadFix
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

instance MonadTransControl (NetworkT t) where
  type StT (NetworkT t) a = StT (ReaderT (NetworkEnv t)) a
  liftWith = defaultLiftWith NetworkT runNetworkT
  restoreT = defaultRestoreT NetworkT

instance MonadBase b m => MonadBase b (NetworkT t m) where
  liftBase = NetworkT . liftBase

instance (MonadBaseControl b m) => MonadBaseControl b (NetworkT t m) where
  type StM (NetworkT t m) a = ComposeSt (NetworkT t) m a
  liftBaseWith     = defaultLiftBaseWith
  restoreM         = defaultRestoreM

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
    s <- newNetworkEnv opts messageE connPeerE disconnPeerE fireDisconnPeer
    a <- runModule (networkNextOptions opts) (runReaderT (runNetworkT m) s)
    processNetEvents s messageFire fireConnPeer fireDisconnPeer
    return a

  withModule t _ = withENetDo . withModule t (Proxy :: Proxy m)

-- | Poll all events from ENet, does nothing when system doesn't have any initiated host object
processNetEvents :: forall t m . MonadAppHost t m
  => NetworkEnv t -- ^ Context that holds reference with host
  -> MessageEventFire -- ^ Action that fires event about incoming message
  -> ConnectPeerFire  -- ^ Action that fires event about connected peer
  -> DisconnectPeerFire -- ^ Action that fires event about disconnected peer
  -> m ()
processNetEvents st msgFire fireConnPeer fireDisconnPeer = do
  rec termatorPrevDyn <- holdAppHost (return noTerminator) $
        ffor (externalEvent $ networkEnvHost st) $ \case
          Nothing -> return noTerminator
          Just host -> do
            terminatorPref <- sample (current termatorPrevDyn)
            liftIO terminatorPref
            tid <- handleEvents host
            return $ killThread tid
  return ()
  where
    noTerminator = return ()
    -- very important
    awaitForEvent = networkPollTimeout $ networkEnvOptions st

    handleEvents :: Host -> m ThreadId
    handleEvents host = liftIO . forkOS $ forever $ do
      me <- service host awaitForEvent
      case me of
        Nothing -> return ()
        Just e -> handleEvent e

    handleEvent :: B.Event -> IO ()
    handleEvent (B.Event et peer ch edata packetPtr) = case et of
      B.None -> detailLog "Network: Event none"
      B.Connect -> do
        detailLog "Network: Peer connected"
        _ <- fireConnPeer peer
        return ()
      B.Disconnect -> do
        detailLog $ "Network: Peer disconnected, code " <> show edata
        _ <- fireDisconnPeer peer
        return ()
      B.Receive -> do
        (Packet !fs !bs) <- peek packetPtr
        detailLog $ "Network: Received message at channel " <> show ch <> ": "
          <> show fs <> ", payload: " <> show bs
        _ <- msgFire (peer, ch, bs)
        return ()

    detailLog = when (networkDetailedLogging $ networkEnvOptions st) . putStrLn

instance {-# OVERLAPPING #-} (MonadBaseControl IO m, MonadCatch m, MonadAppHost t m) => NetworkMonad t (NetworkT t m) where
  networkMessage = asks networkEnvMessageEvent
  {-# INLINE networkMessage #-}

  msgSendM peer chan msg = do
    detailed <- asks (networkDetailedLogging . networkEnvOptions)
    when detailed $ logMsgLnM LogInfo $ "Network: sending packet via channel "
       <> showl chan <> ", payload: " <> showl msg
    let sendAction = liftIO $ P.send peer chan =<< P.poke (messageToPacket msg)
    catch sendAction $ \(e :: IOException) -> do
      logMsgLnM LogError $ "Network: failed to send packet '" <> showl e <> "'"
  {-# INLINE msgSendM #-}

  msgSend e = performAppHost $ ffor e $ \(peer, chan, msg) -> msgSendM peer chan msg
  {-# INLINE msgSend #-}

  networkChannels = externalRefDynamic =<< asks networkEnvMaxChannels
  {-# INLINE networkChannels #-}

  terminateHost = do
    st <- ask
    modifyExternalRefM (networkEnvHost st) $ \case
      Nothing -> return (Nothing, ())
      Just host -> do
        liftIO $ destroy host
        return (Nothing, ())
  {-# INLINE terminateHost #-}

instance {-# OVERLAPPING #-} (MonadBaseControl IO m, MonadCatch m, MonadAppHost t m) => NetworkServer t (NetworkT t m) where
  serverListen e = do
    st <- ask
    detailedDyn <- loggingDebugFlag
    performAppHostAsync $ ffor e $ \ServerListen{..} -> wrapError $ do
      detailed <- sample (current detailedDyn)
      host <- networkBind (Just listenAddress) listenMaxConns listenChanns listenIncoming listenOutcoming detailed
      writeExternalRef (networkEnvHost st) (Just host)
  {-# INLINE serverListen #-}

  peerConnected = asks networkEnvPeerConnect
  {-# INLINE peerConnected #-}

  peerDisconnected = asks newtorkStatePeerDisconnect
  {-# INLINE peerDisconnected #-}

  disconnect e = do
    peers <- networkPeers
    performAppHost $ ffor e $ const $ do
      mapM_ disconnectPeerM =<< sample (current peers)
      disconnectFromServerM
  {-# INLINE disconnect #-}

  disconnectPeerM peer = do
    st <- ask
    liftIO $ do
      P.disconnect peer 0
      _ <- networkEnvPeerDisconnectFire st peer
      return ()
  {-# INLINE disconnectPeerM #-}

  disconnectPeer e = performAppHost $ fmap disconnectPeerM e
  {-# INLINE disconnectPeer #-}

  disconnectPeers e = performAppHost $ fmap (mapM_ disconnectPeerM) e
  {-# INLINE disconnectPeers #-}

  networkPeers = do
    connE <- peerConnected
    dconnE <- peerDisconnected
    foldDyn updatePeers mempty $ align connE dconnE
    where
    updatePeers a peers = case a of
      This conPeer  -> peers S.|> conPeer
      That dconPeer -> S.filter (/= dconPeer) peers
      These conPeer dconPeer -> S.filter (/= dconPeer) (peers S.|> conPeer)
  {-# INLINE networkPeers #-}

instance {-# OVERLAPPING #-} (MonadBaseControl IO m, MonadCatch m, MonadAppHost t m) => NetworkClient t (NetworkT t m) where
  clientConnect e = do
    st <- ask
    detailedDyn <- loggingDebugFlag
    performAppHostAsync $ ffor e $ \ClientConnect{..} -> wrapError $ do
      detailed <- sample (current detailedDyn)
      host <- networkBind Nothing 1 clientChanns clientIncoming clientOutcoming detailed
      writeExternalRef (networkEnvHost st) (Just host)
      serv <- networkConnect host clientAddrr clientChanns 0 detailed
      writeExternalRef (networkEnvServer st) (Just serv)
  {-# INLINE clientConnect #-}

  serverPeer = externalRefDynamic =<< asks networkEnvServer
  {-# INLINE serverPeer #-}

  disconnectFromServerM = do
    st <- ask
    modifyExternalRefM (networkEnvServer st) $ \case
      Nothing -> return (Nothing, ())
      Just serv -> do
        liftIO $ P.disconnectNow serv 0
        return (Nothing, ())
  {-# INLINE disconnectFromServerM #-}

  disconnectFromServer = performAppHost . fmap (const disconnectFromServerM)
  {-# INLINE disconnectFromServer #-}

  connected = asks (fcutMaybe . externalEvent . networkEnvServer)
  {-# INLINE connected #-}

  disconnected = asks (fkeepNothing . externalEvent . networkEnvServer)
  {-# INLINE disconnected #-}

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
      when detailed $ logMsgLnM LogInfo $ case addr of
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
  when (peer == nullPtr) $ throwError $ NetworkConnectFail addr
  when detailed $ logMsgLnM LogInfo $ "Network: connected to " <> showl addr
  return peer