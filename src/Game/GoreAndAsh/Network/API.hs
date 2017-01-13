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
{-# LANGUAGE RecursiveDo #-}
module Game.GoreAndAsh.Network.API(
  -- * Network API
    NetworkMonad(..)
  , peerSend
  , peerChanSend
  , peerSendMany
  , peerChanSendMany
  , terminateNetwork
  , peerMessage
  , chanMessage
  , peerChanMessage
  , HasDisconnect(..)
  -- ** Client API
  , NetworkClient(..)
  , whenConnected
  , whenConnectedWithDisconnect
  -- ** Server API
  , NetworkServer(..)
  -- ** Collections
  , PeerAction(..)
  , peersCollection
  , peersCollectionWithDisconnect
  , processPeers
  , processPeersWithDisconnect
  ) where

import Control.Monad.Catch
import Control.Monad.Except
import Data.ByteString (ByteString)
import Data.Map.Strict (Map)
import Data.Monoid
import Data.Set (Set)
import Game.GoreAndAsh
import Game.GoreAndAsh.Logging
import Game.GoreAndAsh.Network.Backend

import qualified Data.Foldable as F
import qualified Data.Map.Strict as M

-- | API of the network module, shared operations between client and server.
class (MonadIO m, MonadCatch m, MonadFix m, Reflex t, MonadHold t m
     , MonadAppHost t m, LoggingMonad t m, HasNetworkBackend a)
  => NetworkMonad t a m | m -> t, m -> a where
  -- | Fires when a network message is received
  networkMessage :: m (Event t (Peer a, ChannelId, ByteString))

  -- | Sends a packet to given peer on given channel. Constuct time version
  msgSendM :: Peer a -> ChannelId -> MessageType -> ByteString -> m ()

  -- | Sends a packet to given peer on given channel when input event fires
  -- Returns event that fires when sending is complete.
  msgSend :: Event t (Peer a, ChannelId, MessageType, ByteString) -> m (Event t ())

  -- | Sends many packets to given peer on given channel when input event fires
  -- Returns event that fires when sending is complete.
  msgSendMany :: Foldable f => Event t (f (Peer a, ChannelId, MessageType, ByteString)) -> m (Event t ())

  -- | Terminate local host object, terminates network module
  terminateBackend :: m ()

  -- | Fires when a async error is occured in backend
  networkSomeError :: m (Event t (BackendEventError a))

  -- | Fires when a message sending failed
  networkSendError :: m (Event t (SendError a))

  -- | Fires when a connection error occured (incoming or outcoming connection)
  networkConnectionError :: m (Event t (BackendConnectError a, RemoteAddress))

-- | API of the network module, client side operations
class NetworkMonad t a m => NetworkClient t a m | m -> t, m -> a where
  -- | Initiate connection to the remote host
  clientConnect :: Event t (RemoteAddress, ConnectOptions a)
    -> m (Event t (Peer a))

  -- | Connection to remote server (client side). Server side value is always 'Nothing'
  serverPeer :: m (Dynamic t (Maybe (Peer a)))

  -- | Disconnect from remote server (client side)
  disconnectFromServerM :: m ()

  -- | Disconnect from remote server (client side)
  -- Returns event that fires when disconnection is complete.
  disconnectFromServer :: Event t () -> m (Event t ())

  -- | Fires when is connected to remote server
  connected :: m (Event t (Peer a))

  -- | Fires when is disconnected from remote server
  disconnected :: m (Event t ())

-- | API of the network module, server side operations
class NetworkMonad t a m => NetworkServer t a m | m -> t, m -> a where
  -- | Event that fires when a client is connected to server
  peerConnected :: m (Event t (Peer a))

  -- | Event that fires when a client is disconnected from server
  peerDisconnected :: m (Event t (Peer a))

  -- | Disconnect connected peer (server side)
  disconnectPeerM :: Peer a -> m ()

  -- | Disconnect peer (server side).
  -- Returns event that fires when disconnection is complete.
  disconnectPeer :: Event t (Peer a) -> m (Event t ())

  -- | Disconnect many peers (server side).
  -- Returns event that fires when disconnection is complete.
  disconnectPeers :: Foldable f => Event t (f (Peer a)) -> m (Event t ())

  -- | Return collection of connected peers
  networkPeers :: m (Dynamic t (Set (Peer a)))

-- | Automatic lifting across monad stack
instance {-# OVERLAPPABLE #-} (MonadAppHost t (mt m), MonadIO (mt m), MonadCatch (mt m), MonadFix (mt m), MonadHold t (mt m), LoggingMonad t m, NetworkMonad t a m, MonadTrans mt, HasNetworkBackend a) => NetworkMonad t a (mt m) where
  networkMessage = lift networkMessage
  {-# INLINE networkMessage #-}
  msgSendM peer chan mt msg = lift $ msgSendM peer chan mt msg
  {-# INLINE msgSendM #-}
  msgSend e = lift $ msgSend e
  {-# INLINE msgSend #-}
  msgSendMany e = lift $ msgSendMany e
  {-# INLINE msgSendMany #-}
  terminateBackend = lift terminateBackend
  {-# INLINE terminateBackend #-}
  networkSomeError = lift networkSomeError
  {-# INLINE networkSomeError #-}
  networkSendError = lift networkSendError
  {-# INLINE networkSendError #-}
  networkConnectionError = lift networkConnectionError
  {-# INLINE networkConnectionError #-}

instance {-# OVERLAPPABLE #-} (MonadAppHost t (mt m), LoggingMonad t m, NetworkMonad t a m, NetworkClient t a m, MonadCatch (mt m), MonadTrans mt, HasNetworkBackend a) => NetworkClient t a (mt m) where
  clientConnect e = lift $ clientConnect e
  {-# INLINE clientConnect #-}
  serverPeer = lift serverPeer
  {-# INLINE serverPeer #-}
  disconnectFromServerM = lift $ disconnectFromServerM
  {-# INLINE disconnectFromServerM #-}
  disconnectFromServer e = lift $ disconnectFromServer e
  {-# INLINE disconnectFromServer #-}
  connected = lift connected
  {-# INLINE connected #-}
  disconnected = lift disconnected
  {-# INLINE disconnected #-}

instance {-# OVERLAPPABLE #-} (MonadAppHost t (mt m), LoggingMonad t m, NetworkMonad t a m, NetworkServer t a m, MonadCatch (mt m), MonadTrans mt, HasNetworkBackend a) => NetworkServer t a (mt m) where
  peerConnected = lift peerConnected
  {-# INLINE peerConnected #-}
  peerDisconnected = lift peerDisconnected
  {-# INLINE peerDisconnected #-}
  disconnectPeer e = lift $ disconnectPeer e
  {-# INLINE disconnectPeer #-}
  disconnectPeers e = lift $ disconnectPeers e
  {-# INLINE disconnectPeers #-}
  disconnectPeerM e = lift $ disconnectPeerM e
  {-# INLINE disconnectPeerM #-}
  networkPeers = lift networkPeers
  {-# INLINE networkPeers #-}

-- | Variation of 'msgSend' with static peer
peerSend :: NetworkMonad t a m
  => Peer a -- ^ Remote peer to whom send the message
  -> Event t (ChannelId, MessageType, ByteString) -- ^ Message payload
  -> m (Event t ())
peerSend peer e = msgSend $ fmap (\(a, b, c) -> (peer, a, b, c)) e

-- | Variation of 'msgSend' with static peer and channel
peerChanSend :: NetworkMonad t a m
  => Peer a -- ^ Remote peer to whom send the message
  -> ChannelId -- ^ Static ID of channel to use
  -> Event t (MessageType, ByteString) -- ^ Message payload
  -> m (Event t ())
peerChanSend peer chan e = msgSend $ fmap (\(a, b) -> (peer, chan, a, b)) e

-- | Variation of 'msgSendMany' with static peer
peerSendMany :: (NetworkMonad t a m, Foldable f, Functor f)
  => Peer a -> Event t (f (ChannelId, MessageType, ByteString)) -> m (Event t ())
peerSendMany peer e = msgSendMany $ fmap (\(a, b, c) -> (peer, a, b, c)) <$> e

-- | Variation of 'msgSendMany' with static peer and channel
peerChanSendMany :: (NetworkMonad t a m, Foldable f, Functor f)
  => Peer a -> ChannelId -> Event t (f (MessageType, ByteString)) -> m (Event t ())
peerChanSendMany peer chan e = msgSendMany $ fmap (\(a, b) -> (peer, chan, a, b)) <$> e

-- | Specialisation of 'networkMessage' event for given peer
peerMessage :: NetworkMonad t a m => Peer a -> m (Event t (ChannelId, ByteString))
peerMessage peer = do
  emsg <- networkMessage
  return $ fforMaybe emsg $ \(msgPeer, ch, bs) -> if msgPeer == peer
    then Just (ch, bs)
    else Nothing

-- | Specialisation of 'networkMessage' event for given cahnnel
chanMessage :: NetworkMonad t a m => ChannelId -> m (Event t (Peer a, ByteString))
chanMessage chan = do
  emsg <- networkMessage
  return $ fforMaybe emsg $ \(peer, ch, bs) -> if chan == ch
    then Just (peer, bs)
    else Nothing

-- | Specialisation of 'networkMessage' event for given peer and channel
peerChanMessage :: NetworkMonad t a m => Peer a -> ChannelId -> m (Event t ByteString)
peerChanMessage peer chan = do
  emsg <- networkMessage
  return $ fforMaybe emsg $ \(msgPeer, ch, bs) -> if msgPeer == peer && ch == chan
    then Just bs
    else Nothing

-- | Terminate all connections and destroy host object
terminateNetwork :: NetworkServer t a m => Event t () -> m (Event t ())
terminateNetwork e = do
  peers <- networkPeers
  performAppHost $ ffor e $ const $ do
    mapM_ disconnectPeerM =<< sample (current peers)
    terminateBackend

-- | Action with 'Peer' in 'peersCollection'
data PeerAction = PeerRemove | PeerAdd

-- | Create component for each connected peer and automatically handle peer
-- connection and disconnection. The function has special input to manually
-- adding and removing peers from collection.
peersCollection :: NetworkServer t a m
  => Event t (Map (Peer a) PeerAction) -- ^ Fires when user wants to add/remove peers from collection
  -> (Peer a -> PushM t Bool) -- ^ Cheker that is run each time new peer is connected that indicates whether to add the peer to the collection or not.
  -> (Peer a -> m b) -- ^ Widget of single peer
  -> m (Dynamic t (Map (Peer a) b)) -- ^ Collected output of all currently active peers
peersCollection manualE peerChecker handlePeer = do
  -- transform manual actions
  let manualE' = fmap converAction <$> manualE
  -- sample current set of peers
  peersSeq <- sample . current =<< networkPeers
  let initialPeers = M.fromList $ fmap (, ()) $ F.toList peersSeq
  -- listen connected peers
  connE <- peerConnected
  let connE' = flip push connE $ \p -> do
        res <- peerChecker p
        return $ if res then Just p else Nothing
  let addE = ffor connE' $ \p -> M.singleton p (Just ())
  -- listen disconnected peers
  discE <- peerDisconnected
  -- Merge all sources of peers into collection
  let delE = ffor discE $ \p -> M.singleton p (Nothing)
      updE = addE <> delE <> manualE'
  holdKeyCollection initialPeers updE (\p _ -> handlePeer p)
  where
    converAction action = case action of
      PeerRemove -> Nothing
      PeerAdd -> Just ()

-- | Create component for each connected peer and automatically handle peer
-- connection and disconnection.
processPeers :: NetworkServer t a m => (Peer a -> m b) -> m (Dynamic t (Map (Peer a) b))
processPeers = peersCollection never (const $ pure True)

-- | Defines that 'a' structure has disconnect event
class HasDisconnect a where
  getDisconnect :: a -> Event t ()

-- | Create component for each connected peer and automatically handle peer
-- connection and disconnection. The function has special input to manually
-- adding and removing peers from collection.
--
-- In distinct from 'processPeers' the components inside the collection can
-- disconnect themselves.
peersCollectionWithDisconnect :: (NetworkServer t a m, HasDisconnect b)
  => Event t (Map (Peer a) PeerAction) -- ^ Fires when user wants to add/remove peers from collection
  -> (Peer a -> PushM t Bool) -- ^ Cheker that is run each time new peer is connected that indicates whether to add the peer to the collection or not.
  -> (Peer a -> m b) -- ^ Component for each peer
  -> m (Dynamic t (Map (Peer a) b)) -- ^ Collected output of peers components
peersCollectionWithDisconnect manualE peerChecker handlePeer = do
  -- convert manual events
  let manualE' = fmap converAction <$> manualE
  -- sample current set of peers
  peersSeq <- sample . current =<< networkPeers
  let initialPeers = M.fromList $ fmap (, ()) $ F.toList peersSeq
  -- listen connected peers
  connE <- peerConnected
  let connE' = flip push connE $ \p -> do
        res <- peerChecker p
        return $ if res then Just p else Nothing
  let addE = ffor connE' $ \p -> M.singleton p (Just ())
  -- listen disconnected peers
  discE <- peerDisconnected
  let delE = ffor discE $ \p -> M.singleton p (Nothing)
  -- recursive loop for tracking self disconnection
  rec
    outputsDyn <- holdKeyCollection initialPeers updE (\p _ -> handlePeer p)
    let selfDelDyn = fmap (fmap (const Nothing) . getDisconnect) <$> outputsDyn
        selfDelE = switchPromptlyDyn $ mergeMap <$> selfDelDyn
        updE = addE <> delE <> selfDelE <> manualE'
  _ <- disconnectPeers $ fmap M.keys selfDelE
  return outputsDyn
  where
    converAction action = case action of
      PeerRemove -> Nothing
      PeerAdd -> Just ()

-- | Create component for each connected peer and automatically handle peer
-- connection and disconnection.
--
-- In distinct from 'processPeers' the components inside the collection can
-- disconnect themselves.
processPeersWithDisconnect :: (NetworkServer t a m, HasDisconnect b)
  => (Peer a -> m b) -> m (Dynamic t (Map (Peer a) b))
processPeersWithDisconnect = peersCollectionWithDisconnect never (const $ pure True)

-- | Switch to provided component when client is connected to server.
whenConnected :: NetworkClient t a m
  => m b -- ^ The component is used when client is not connected (yet or already)
  -> (Peer a -> m b) -- ^ The component is used when client is connected to server
  -> m (Dynamic t b) -- ^ Collected result from both stages.
whenConnected whenDown m = do
  -- Check if the server is already exising at build time
  curServerDyn <- serverPeer
  curServer <- sample . current $ curServerDyn
  initVal <- case curServer of
    Just server -> m server
    Nothing -> whenDown
  -- Dynamic changing of server with rebuilding
  serverE <- connected
  disconE <- disconnected
  let updE = leftmost [fmap m serverE, fmap (const whenDown) disconE]
  holdAppHost (pure initVal) updE

-- | Switch to provided component when client is connected to server.
--
-- Same as 'whenConnected' but provides ability to self disconnect for the client.
whenConnectedWithDisconnect :: (NetworkClient t a m, HasDisconnect b)
  => m b -- ^ The component is used when client is not connected (yet or already)
  -> (Peer a -> m b) -- ^ The component is used when client is connected to server
  -> m (Dynamic t b) -- ^ Collected result from both stages.
whenConnectedWithDisconnect whenDown m = do
  serverE <- connected
  disconE <- disconnected
  let externalE = leftmost [fmap m serverE, fmap (const whenDown) disconE]
  rec
    resDyn <- holdAppHost whenDown updE
    let selfDiscE = switchPromptlyDyn $ getDisconnect <$> resDyn
        updE = leftmost [externalE, fmap (const whenDown) selfDiscE]
  return resDyn
