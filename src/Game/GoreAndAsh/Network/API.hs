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
  , terminateNetwork
  , peerMessage
  , chanMessage
  , peerChanMessage
  , HasDisconnect(..)
  -- ** Client API
  , NetworkClient(..)
  , ClientConnect(..)
  , whenConnected
  , whenConnectedWithDisconnect
  -- ** Server API
  , NetworkServer(..)
  , ServerListen(..)
  -- ** Collections
  , PeerAction(..)
  , peersCollection
  , peersCollectionWithDisconnect
  , processPeers
  , processPeersWithDisconnect
  ) where

import Control.Monad.Catch
import Control.Monad.Except
import Data.Map.Strict (Map)
import Data.Monoid
import Foreign
import Game.GoreAndAsh
import Game.GoreAndAsh.Logging
import Game.GoreAndAsh.Network.Error
import Game.GoreAndAsh.Network.Message
import Game.GoreAndAsh.Network.State
import GHC.Generics
import Network.ENet.Bindings (ChannelID)
import Network.Socket (SockAddr)

import qualified Data.ByteString as BS
import qualified Data.Foldable as F
import qualified Data.Map.Strict as M
import qualified Data.Set as S

-- | Parameters for 'serverListen'
data ServerListen = ServerListen {
  listenAddress   :: !SockAddr -- ^ Address to listen, can be '0.0.0.0'
, listenMaxConns  :: !Word -- ^ Maximum count of connections
, listenChanns    :: !Word -- ^ Number of channels to open
, listenIncoming  :: !Word32 -- ^ Incoming max bandwidth
, listenOutcoming :: !Word32 -- ^ Outcoming max bandwidth
} deriving (Generic)

-- | Parameters for 'clientConnect'
data ClientConnect = ClientConnect {
  clientAddrr     :: !SockAddr -- ^ Address of host
, clientChanns    :: !Word -- ^ Count of channels to open
, clientIncoming  :: !Word32 -- ^ Incoming max bandwidth
, clientOutcoming :: !Word32 -- ^ Outcoming max bandwidth
} deriving (Generic)

-- | API of the network module, shared operations between client and server.
--
-- Operations can throw 'MonadError' exception.
class (MonadIO m, MonadCatch m, MonadFix m, Reflex t, MonadHold t m
     , MonadAppHost t m)
  => NetworkMonad t m | m -> t where
  -- | Fires when a network message is received
  networkMessage :: m (Event t (Peer, ChannelID, BS.ByteString))

  -- | Sends a packet to given peer on given channel. Constuct time version
  msgSendM :: LoggingMonad t m => Peer -> ChannelID -> Message -> m ()

  -- | Sends a packet to given peer on given channel when input event fires
  -- Returns event that fires when sending is complete.
  msgSend :: LoggingMonad t m => Event t (Peer, ChannelID, Message) -> m (Event t ())

  -- | Sends many packets to given peer on given channel when input event fires
  -- Returns event that fires when sending is complete.
  msgSendMany :: (LoggingMonad t m, Foldable f) => Event t (f (Peer, ChannelID, Message)) -> m (Event t ())

  -- | Return count of allocated network channels
  networkChannels :: m (Dynamic t Word)

  -- | Terminate local host object, terminates network module
  terminateHost :: m ()

-- | API of the network module, client side operations
class NetworkMonad t m => NetworkClient t m | m -> t where
  -- | Initiate connection to the remote host
  clientConnect :: LoggingMonad t m
    => Event t ClientConnect -- ^ Input parameters
    -> m (Event t (Either NetworkError ()))

  -- | Connection to remote server (client side). Server side value is always 'Nothing'
  serverPeer :: m (Dynamic t (Maybe Peer))

  -- | Disconnect from remote server (client side)
  disconnectFromServerM :: m ()

  -- | Disconnect from remote server (client side)
  -- Returns event that fires when disconnection is complete.
  disconnectFromServer :: Event t () -> m (Event t ())

  -- | Fires when is connected to remote server
  connected :: m (Event t Peer)

  -- | Fires when is disconnected from remote server
  disconnected :: m (Event t ())

-- | API of the network module, server side operations
class NetworkMonad t m => NetworkServer t m | m -> t where
  -- | Start listening for messages, initialise internal host object
  serverListen :: LoggingMonad t m
    => Event t ServerListen -- ^ Input parameters
    -> m (Event t (Either NetworkError ()))

  -- | Event that fires when a client is connected to server
  peerConnected :: m (Event t Peer)

  -- | Event that fires when a client is disconnected from server
  peerDisconnected :: m (Event t Peer)

  -- | Disconnects all users for server side.
  -- Returns event that fires when disconnection is complete.
  disconnect :: Event t a -> m (Event t ())

  -- | Disconnect connected peer (server side)
  disconnectPeerM :: Peer -> m ()

  -- | Disconnect peer (server side).
  -- Returns event that fires when disconnection is complete.
  disconnectPeer :: Event t Peer -> m (Event t ())

  -- | Disconnect many peers (server side).
  -- Returns event that fires when disconnection is complete.
  disconnectPeers :: Foldable f => Event t (f Peer) -> m (Event t ())

  -- | Return collection of connected peers
  networkPeers :: m (Dynamic t (S.Set Peer))

-- | Automatic lifting across monad stack
instance {-# OVERLAPPABLE #-} (MonadAppHost t (mt m), MonadIO (mt m), MonadCatch (mt m), MonadFix (mt m), MonadHold t (mt m), LoggingMonad t m, NetworkMonad t m, MonadTrans mt) => NetworkMonad t (mt m) where
  networkMessage = lift networkMessage
  msgSendM peer chan msg = lift $ msgSendM peer chan msg
  msgSend e = lift $ msgSend e
  msgSendMany e = lift $ msgSendMany e
  networkChannels = lift networkChannels
  terminateHost = lift terminateHost
  {-# INLINE networkMessage #-}
  {-# INLINE msgSendM #-}
  {-# INLINE msgSend #-}
  {-# INLINE msgSendMany #-}
  {-# INLINE networkChannels #-}
  {-# INLINE terminateHost #-}

instance {-# OVERLAPPABLE #-} (MonadAppHost t (mt m), LoggingMonad t m, NetworkMonad t m, NetworkClient t m, MonadCatch (mt m), MonadTrans mt) => NetworkClient t (mt m) where
  clientConnect = lift . clientConnect
  serverPeer = lift serverPeer
  disconnectFromServerM = lift $ disconnectFromServerM
  disconnectFromServer e = lift $ disconnectFromServer e
  connected = lift connected
  disconnected = lift disconnected
  {-# INLINE clientConnect #-}
  {-# INLINE serverPeer #-}
  {-# INLINE disconnectFromServerM #-}
  {-# INLINE disconnectFromServer #-}
  {-# INLINE connected #-}
  {-# INLINE disconnected #-}

instance {-# OVERLAPPABLE #-} (MonadAppHost t (mt m), LoggingMonad t m, NetworkMonad t m, NetworkServer t m, MonadCatch (mt m), MonadTrans mt) => NetworkServer t (mt m) where
  serverListen = lift . serverListen
  peerConnected = lift peerConnected
  peerDisconnected = lift peerDisconnected
  disconnect e = lift $ disconnect e
  disconnectPeer e = lift $ disconnectPeer e
  disconnectPeers e = lift $ disconnectPeers e
  disconnectPeerM e = lift $ disconnectPeerM e
  networkPeers = lift networkPeers
  {-# INLINE serverListen #-}
  {-# INLINE peerConnected #-}
  {-# INLINE peerDisconnected #-}
  {-# INLINE disconnect #-}
  {-# INLINE disconnectPeer #-}
  {-# INLINE disconnectPeers #-}
  {-# INLINE disconnectPeerM #-}
  {-# INLINE networkPeers #-}

-- | Variation of 'msgSend' with static peer
peerSend :: (LoggingMonad t m, NetworkMonad t m)
  => Peer -> Event t (ChannelID, Message) -> m (Event t ())
peerSend peer e = msgSend $ fmap (\(a, b) -> (peer, a, b)) e

-- | Variation of 'msgSend' with static peer and channel
peerChanSend :: (LoggingMonad t m, NetworkMonad t m)
  => Peer -> ChannelID -> Event t Message -> m (Event t ())
peerChanSend peer chan e = msgSend $ fmap (\a -> (peer, chan, a)) e

-- | Specialisation of 'networkMessage' event for given peer
peerMessage :: NetworkMonad t m => Peer -> m (Event t (ChannelID, BS.ByteString))
peerMessage peer = do
  emsg <- networkMessage
  return $ fforMaybe emsg $ \(msgPeer, ch, bs) -> if msgPeer == peer
    then Just (ch, bs)
    else Nothing

-- | Specialisation of 'networkMessage' event for given cahnnel
chanMessage :: NetworkMonad t m => ChannelID -> m (Event t (Peer, BS.ByteString))
chanMessage chan = do
  emsg <- networkMessage
  return $ fforMaybe emsg $ \(peer, ch, bs) -> if chan == ch
    then Just (peer, bs)
    else Nothing

-- | Specialisation of 'networkMessage' event for given peer and channel
peerChanMessage :: NetworkMonad t m => Peer -> ChannelID -> m (Event t BS.ByteString)
peerChanMessage peer chan = do
  emsg <- networkMessage
  return $ fforMaybe emsg $ \(msgPeer, ch, bs) -> if msgPeer == peer && ch == chan
    then Just bs
    else Nothing

-- | Terminate all connections and destroy host object
terminateNetwork :: NetworkServer t m => Event t () -> m (Event t ())
terminateNetwork e = do
  peers <- networkPeers
  performAppHost $ ffor e $ const $ do
    mapM_ disconnectPeerM =<< sample (current peers)
    terminateHost

-- | Action with 'Peer' in 'peersCollection'
data PeerAction = PeerRemove | PeerAdd

-- | Create component for each connected peer and automatically handle peer
-- connection and disconnection. The function has special input to manually
-- adding and removing peers from collection.
peersCollection :: NetworkServer t m
  => Event t (Map Peer PeerAction) -- ^ Fires when user wants to add/remove peers from collection
  -> (Peer -> PushM t Bool) -- ^ Cheker that is run each time new peer is connected that indicates whether to add the peer to the collection or not.
  -> (Peer -> m a) -- ^ Widget of single peer
  -> m (Dynamic t (Map Peer a)) -- ^ Collected output of all currently active peers
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
processPeers :: NetworkServer t m => (Peer -> m a) -> m (Dynamic t (Map Peer a))
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
peersCollectionWithDisconnect :: (NetworkServer t m, HasDisconnect a)
  => Event t (Map Peer PeerAction) -- ^ Fires when user wants to add/remove peers from collection
  -> (Peer -> PushM t Bool) -- ^ Cheker that is run each time new peer is connected that indicates whether to add the peer to the collection or not.
  -> (Peer -> m a) -- ^ Component for each peer
  -> m (Dynamic t (Map Peer a)) -- ^ Collected output of peers components
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
processPeersWithDisconnect :: (NetworkServer t m, HasDisconnect a)
  => (Peer -> m a) -> m (Dynamic t (Map Peer a))
processPeersWithDisconnect = peersCollectionWithDisconnect never (const $ pure True)

-- | Switch to provided component when client is connected to server.
whenConnected :: NetworkClient t m
  => m a -- ^ The component is used when client is not connected (yet or already)
  -> (Peer -> m a) -- ^ The component is used when client is connected to server
  -> m (Dynamic t a) -- ^ Collected result from both stages.
whenConnected whenDown m = do
  -- Check if the server is already exising at build time
  curServer <- serverPeer
  buildE <- getPostBuild
  let alreadyE = attachPromptlyDynWithMaybe (\mserv _ -> mserv) curServer buildE
  -- Events when server connected/disconnected in future
  serverE <- connected
  disconE <- disconnected
  let updE = leftmost [fmap m serverE, fmap (const whenDown) disconE, fmap m alreadyE]
  holdAppHost whenDown updE

-- | Switch to provided component when client is connected to server.
--
-- Same as 'whenConnected' but provides ability to self disconnect for the client.
whenConnectedWithDisconnect :: (NetworkClient t m, HasDisconnect a)
  => m a -- ^ The component is used when client is not connected (yet or already)
  -> (Peer -> m a) -- ^ The component is used when client is connected to server
  -> m (Dynamic t a) -- ^ Collected result from both stages.
whenConnectedWithDisconnect whenDown m = do
  serverE <- connected
  disconE <- disconnected
  let externalE = leftmost [fmap m serverE, fmap (const whenDown) disconE]
  rec
    resDyn <- holdAppHost whenDown updE
    let selfDiscE = switchPromptlyDyn $ getDisconnect <$> resDyn
        updE = leftmost [externalE, fmap (const whenDown) selfDiscE]
  return resDyn