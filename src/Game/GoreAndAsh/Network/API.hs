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
  , ServerListen(..)
  , ClientConnect(..)
  , peerSend'
  , peerSend''
  , terminateNetwork
  -- * Collections
  , processPeers
  , HasDisconnect(..)
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
import qualified Data.Map.Strict as M
import qualified Data.Sequence as S

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

-- | API of the network module
class (MonadIO m, MonadCatch m, MonadFix m, Reflex t, MonadHold t m
     , MonadError NetworkError m, MonadAppHost t m)
  => NetworkMonad t m | m -> t where
  -- | Start listening for messages, initialise internal host object
  serverListen :: LoggingMonad t m
    => Event t ServerListen -- ^ Input parameters
    -> m (Event t (Either NetworkError ()))

  -- | Initiate connection to the remote host
  clientConnect :: LoggingMonad t m
    => Event t ClientConnect -- ^ Input parameters
    -> m (Event t (Either NetworkError ()))

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

  -- | Disconnect many peers (server side).
  -- Returns event that fires when disconnection is complete.
  disconnectPeers :: Foldable f => Event t (f Peer) -> m (Event t ())

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

  -- | Return collection of connected peers
  networkPeers :: m (Dynamic t (S.Seq Peer))

  -- | Fires when is connected to remote server
  connected :: m (Event t Peer)

  -- | Fires when is disconnected from remote server
  disconnected :: m (Event t ())

-- | Automatic lifting across monad stack
instance {-# OVERLAPPABLE #-} (MonadAppHost t (mt m), MonadIO (mt m), MonadCatch (mt m), MonadFix (mt m), MonadHold t (mt m), LoggingMonad t m, NetworkMonad t m, MonadTrans mt, MonadError NetworkError (mt m)) => NetworkMonad t (mt m) where
  serverListen = lift . serverListen
  clientConnect = lift . clientConnect
  serverPeer = lift serverPeer
  peerConnected = lift peerConnected
  peerDisconnected = lift peerDisconnected
  disconnect e = lift $ disconnect e
  disconnectPeer e = lift $ disconnectPeer e
  disconnectPeers e = lift $ disconnectPeers e
  disconnectPeerM e = lift $ disconnectPeerM e
  disconnectFromServerM = lift $ disconnectFromServerM
  disconnectFromServer e = lift $ disconnectFromServer e
  networkMessage = lift networkMessage
  peerSendM peer chan msg = lift $ peerSendM peer chan msg
  peerSend e = lift $ peerSend e
  networkChannels = lift networkChannels
  terminateHost = lift terminateHost
  networkPeers = lift networkPeers
  connected = lift connected
  disconnected = lift disconnected
  {-# INLINE serverListen #-}
  {-# INLINE clientConnect #-}
  {-# INLINE serverPeer #-}
  {-# INLINE peerConnected #-}
  {-# INLINE peerDisconnected #-}
  {-# INLINE disconnect #-}
  {-# INLINE disconnectPeer #-}
  {-# INLINE disconnectPeers #-}
  {-# INLINE disconnectPeerM #-}
  {-# INLINE disconnectFromServerM #-}
  {-# INLINE disconnectFromServer #-}
  {-# INLINE networkMessage #-}
  {-# INLINE peerSendM #-}
  {-# INLINE peerSend #-}
  {-# INLINE networkChannels #-}
  {-# INLINE terminateHost #-}
  {-# INLINE networkPeers #-}
  {-# INLINE connected #-}
  {-# INLINE disconnected #-}

-- | Variation of 'peerSend' with static peer
peerSend' :: (LoggingMonad t m, NetworkMonad t m)
  => Peer -> Event t (ChannelID, Message) -> m (Event t ())
peerSend' peer e = peerSend $ fmap (\(a, b) -> (peer, a, b)) e

-- | Variation of 'peerSend' with static peer and channel
peerSend'' :: (LoggingMonad t m, NetworkMonad t m)
  => Peer -> ChannelID -> Event t Message -> m (Event t ())
peerSend'' peer chan e = peerSend $ fmap (\a -> (peer, chan, a)) e

-- | Terminate all connections and destroy host object
terminateNetwork :: NetworkMonad t m => Event t () -> m (Event t ())
terminateNetwork e = do
  peers <- networkPeers
  performAppHost $ ffor e $ const $ do
    mapM_ disconnectPeerM =<< sample (current peers)
    disconnectFromServerM
    terminateHost

-- | Create component for each connected peer and automatically handle peer
-- connection and disconnection.
processPeers :: NetworkMonad t m => (Peer -> m a) -> m (Dynamic t (Map Peer a))
processPeers handlePeer = do
  connE <- peerConnected
  let addE = ffor connE $ \p -> M.singleton p (Just ())
  discE <- peerDisconnected
  let delE = ffor discE $ \p -> M.singleton p (Nothing)
      updE = addE <> delE
  holdKeyCollection mempty updE (\p _ -> handlePeer p)

-- | Defines that 'a' structure has disconnect event
class HasDisconnect a where
  getDisconnect :: a -> Event t ()

-- | Create component for each connected peer and automatically handle peer
-- connection and disconnection.
--
-- In distinct from 'processPeers' the components inside the collection can
-- disconnect themself.
processPeersWithDisconnect :: (NetworkMonad t m, HasDisconnect a)
  => (Peer -> m a) -> m (Dynamic t (Map Peer a))
processPeersWithDisconnect handlePeer = do
  connE <- peerConnected
  let addE = ffor connE $ \p -> M.singleton p (Just ())
  discE <- peerDisconnected
  let delE = ffor discE $ \p -> M.singleton p (Nothing)
  rec
    outputsDyn <- holdKeyCollection mempty updE (\p _ -> handlePeer p)
    let selfDelDyn = fmap (fmap (const Nothing) . getDisconnect) <$> outputsDyn
        selfDelE = switchPromptlyDyn $ mergeMap <$> selfDelDyn
        updE = addE <> delE <> selfDelE
  _ <- disconnectPeers $ fmap M.keys selfDelE
  return outputsDyn
