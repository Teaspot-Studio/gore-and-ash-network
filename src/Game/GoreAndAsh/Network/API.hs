{-|
Module      : Game.GoreAndAsh.Network.API
Description : Monadic and arrow API for network core module
Copyright   : (c) Anton Gushcha, 2015-2016
License     : BSD3
Maintainer  : ncrashed@gmail.com
Stability   : experimental
Portability : POSIX

The module contains monadic and arrow API of network core module.
-}
{-# LANGUAGE LambdaCase #-}
module Game.GoreAndAsh.Network.API(
  -- * Network API
    NetworkMonad(..)
  , connected
  , disconnected
  , peerSend'
  , peerSend''
  , terminateNetwork
  ) where

import Control.Exception.Base (IOException)
import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.Reader
import Data.Align
import Data.Monoid
import Data.These
import Foreign
import Game.GoreAndAsh
import Game.GoreAndAsh.Logging
import Game.GoreAndAsh.Network.Error
import Game.GoreAndAsh.Network.Message
import Game.GoreAndAsh.Network.Module
import Game.GoreAndAsh.Network.Options
import Game.GoreAndAsh.Network.State
import Network.ENet.Bindings (ChannelID)
import Network.ENet.Host
import Network.Socket (SockAddr)

import qualified Data.ByteString as BS
import qualified Data.Sequence as S
import qualified Network.ENet.Packet as P
import qualified Network.ENet.Peer as P

-- | API of the network module
class (MonadIO m, MonadCatch m, MonadFix m, Reflex t, MonadHold t m) => NetworkMonad t m | m -> t where
  -- | Start listening for messages, initialise internal host object
  serverListen :: (LoggingMonad t m, MonadError NetworkError m)
    => SockAddr -- ^ Address to listen, can be '0.0.0.0'
    -> Word -- ^ Maximum count of connections
    -> Word -- ^ Number of channels to open
    -> Word32 -- ^ Incoming max bandwidth
    -> Word32 -- ^ Outcoming max bandwidth
    -> m ()

  -- | Initiate connection to the remote host
  clientConnect :: (LoggingMonad t m, MonadError NetworkError m)
    => SockAddr -- ^ Address of host
    -> Word -- ^ Count of channels to open
    -> Word32 -- ^ Incoming max bandwidth
    -> Word32 -- ^ Outcoming max bandwidth
    -> m ()

  -- | Connection to remote server (client side). Server side value is always 'Nothing'
  serverPeer :: m (Dynamic t (Maybe Peer))

  -- | Event that fires when a client is connected to server
  peerConnected :: m (Event t Peer)

  -- | Event that fires when a client is disconnected from server
  peerDisconnected :: m (Event t Peer)

  -- | Disconnect from remote server (client side). Disconnects all users for server side.
  -- Returns event that fires when disconnection is complete.
  disconnect :: Event t a -> m (Event t ())

  -- | Disconnect connected peer (server side)
  disconnectPeerM :: Peer -> m ()

  -- | Disconnect peer (server side).
  -- Returns event that fires when disconnection is complete.
  disconnectPeer :: Event t Peer -> m (Event t ())

  -- | Disconnect from remote server (client side)
  disconnectFromServerM :: m ()

  -- | Disconnect from remote server (client side)
  -- Returns event that fires when disconnection is complete.
  disconnectFromServer :: Event t () -> m (Event t ())

  -- | Fires when a network message is received
  networkMessage :: m (Event t (Peer, ChannelID, BS.ByteString))

  -- | Sends a packet to given peer on given channel. Constuct time version
  peerSendM :: LoggingMonad t m => Peer -> ChannelID -> Message -> m ()

  -- | Sends a packet to given peer on given channel when input event fires
  -- Returns event that fires when sending is complete.
  peerSend :: LoggingMonad t m => Event t (Peer, ChannelID, Message) -> m (Event t ())

  -- | Return count of allocated network channels
  networkChannels :: m (Dynamic t Word)

  -- | Terminate local host object, terminates network module
  terminateHost :: m ()

instance {-# OVERLAPPING #-} (MonadIO m, MonadCatch m, MonadAppHost t m) => NetworkMonad t (NetworkT t m) where
  serverListen addr conCount chanCount inBandth outBandth = do
    st <- ask
    let detailed = networkDetailedLogging $ networkStateOptions st
    host <- networkBind (Just addr) conCount chanCount inBandth outBandth detailed
    writeExternalRef (networkStateHost st) (Just host)
  {-# INLINE serverListen #-}

  clientConnect addr chanCount inBandth outBandth = do
    st <- ask
    let detailed = networkDetailedLogging $ networkStateOptions st
    host <- networkBind Nothing 1 chanCount inBandth outBandth detailed
    serv <- networkConnect host addr chanCount 0 detailed
    writeExternalRef (networkStateServer st) (Just serv)
  {-# INLINE clientConnect #-}

  serverPeer = externalRefDynamic =<< asks networkStateServer
  {-# INLINE serverPeer #-}

  peerConnected = asks networkStatePeerConnect
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
      _ <- networkStatePeerDisconnectFire st peer
      return ()
  {-# INLINE disconnectPeerM #-}

  disconnectPeer e = performAppHost $ fmap disconnectPeerM e
  {-# INLINE disconnectPeer #-}

  disconnectFromServerM = do
    st <- ask
    modifyExternalRefM (networkStateServer st) $ \case
      Nothing -> return (Nothing, ())
      Just serv -> do
        liftIO $ P.disconnectNow serv 0
        return (Nothing, ())
  {-# INLINE disconnectFromServerM #-}

  disconnectFromServer = performAppHost . fmap (const disconnectFromServerM)
  {-# INLINE disconnectFromServer #-}

  networkMessage = asks networkStateMessageEvent
  {-# INLINE networkMessage #-}

  peerSendM peer chan msg = do
    detailed <- asks (networkDetailedLogging . networkStateOptions)
    when detailed $ logMsgMLn LogInfo $ "Network: sending packet via channel "
       <> showl chan <> ", payload: " <> showl msg
    let sendAction = liftIO $ P.send peer chan =<< P.poke (messageToPacket msg)
    catch sendAction $ \(e :: IOException) -> do
      logMsgMLn LogError $ "Network: failed to send packet '" <> showl e <> "'"
  {-# INLINE peerSendM #-}

  peerSend e = performAppHost $ ffor e $ \(peer, chan, msg) -> peerSendM peer chan msg
  {-# INLINE peerSend #-}

  networkChannels = externalRefDynamic =<< asks networkStateMaxChannels
  {-# INLINE networkChannels #-}

  terminateHost = do
    st <- ask
    modifyExternalRefM (networkStateHost st) $ \case
      Nothing -> return (Nothing, ())
      Just host -> do
        liftIO $ destroy host
        return (Nothing, ())
  {-# INLINE terminateHost #-}

-- | Automatic lifting across monad stack
instance {-# OVERLAPPABLE #-} (MonadIO (mt m), MonadCatch (mt m), MonadFix (mt m), MonadHold t (mt m), LoggingMonad t m, NetworkMonad t m, MonadTrans mt, MonadError NetworkError m) => NetworkMonad t (mt m) where
  serverListen addr conCount chanCount inBandth outBandth = lift $ serverListen addr conCount chanCount inBandth outBandth
  clientConnect addr chanCount inBandth outBandth = lift $ clientConnect addr chanCount inBandth outBandth
  serverPeer = lift serverPeer
  peerConnected = lift peerConnected
  peerDisconnected = lift peerDisconnected
  disconnect e = lift $ disconnect e
  disconnectPeer e = lift $ disconnectPeer e
  disconnectPeerM e = lift $ disconnectPeerM e
  disconnectFromServerM = lift $ disconnectFromServerM
  disconnectFromServer e = lift $ disconnectFromServer e
  networkMessage = lift networkMessage
  peerSendM peer chan msg = lift $ peerSendM peer chan msg
  peerSend e = lift $ peerSend e
  networkChannels = lift networkChannels
  terminateHost = lift terminateHost
  {-# INLINE serverListen #-}
  {-# INLINE clientConnect #-}
  {-# INLINE serverPeer #-}
  {-# INLINE peerConnected #-}
  {-# INLINE peerDisconnected #-}
  {-# INLINE disconnect #-}
  {-# INLINE disconnectPeer #-}
  {-# INLINE disconnectPeerM #-}
  {-# INLINE disconnectFromServerM #-}
  {-# INLINE disconnectFromServer #-}
  {-# INLINE networkMessage #-}
  {-# INLINE peerSendM #-}
  {-# INLINE peerSend #-}
  {-# INLINE networkChannels #-}
  {-# INLINE terminateHost #-}

-- | Return collection of connected peers
networkPeers :: NetworkMonad t m => m (Dynamic t (S.Seq Peer))
networkPeers = do
  connE <- peerConnected
  dconnE <- peerDisconnected
  foldDyn updatePeers mempty $ align connE dconnE
  where
  updatePeers a peers = case a of
    This conPeer  -> peers S.|> conPeer
    That dconPeer -> S.filter (/= dconPeer) peers
    These conPeer dconPeer -> S.filter (/= dconPeer) (peers S.|> conPeer)

-- | Fires when is connected to remote server
connected :: NetworkMonad t m => m (Event t Peer)
connected = undefined

-- | Fires when is disconnected from remote server
disconnected :: NetworkMonad t m => m (Event t ())
disconnected = undefined

-- | Variation of 'peerSend' with static peer
peerSend' :: (LoggingMonad t m, NetworkMonad t m)
  => Peer -> Event t (ChannelID, Message) -> m (Event t ())
peerSend' peer e = peerSend $ fmap (\(a, b) -> (peer, a, b)) e

-- | Variation of 'peerSend' with static peer and channel
peerSend'' :: (LoggingMonad t m, NetworkMonad t m)
  => Peer -> ChannelID -> Event t Message -> m (Event t ())
peerSend'' peer chan e = peerSend $ fmap (\a -> (peer, chan, a)) e

-- | Terminate all connections and destroy host object
terminateNetwork :: (MonadAppHost t m, NetworkMonad t m) => Event t () -> m (Event t ())
terminateNetwork e = do
  peers <- networkPeers
  performAppHost $ ffor e $ const $ do
    mapM_ disconnectPeerM =<< sample (current peers)
    disconnectFromServerM
    terminateHost

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
