{-|
Module      : Game.GoreAndAsh.Network
Description : Network backend API
Copyright   : (c) Anton Gushcha, 2015-2017
License     : BSD3
Maintainer  : ncrashed@gmail.com
Stability   : experimental
Portability : POSIX
-}
module Game.GoreAndAsh.Network.Backend(
    ChannelId(..)
  , RemoteAddress
  , MessageType(..)
  , NetworkBackendContext(..)
  , NetworkBackend(..)
  , HasNetworkBackend(..)
  , SendError(..)
  ) where

import Control.DeepSeq
import Control.Monad.IO.Class
import Data.ByteString (ByteString)
import Data.Typeable
import Data.Word
import GHC.Generics

-- | Channel ID for network connection. Channels can be used to separate data
-- streams with different format.
newtype ChannelId = ChannelId { unChannelId :: Word32 }
  deriving (Generic, Show, Eq, Ord)

instance NFData ChannelId

-- | Abstract remote node address. It could be IP + port, or hostname, or
-- name of unix socket, etc.
type RemoteAddress = ByteString

-- | Strategy how given message is delivered to remote host
data MessageType =
    ReliableMessage -- ^ TCP like, ordered reliable delivery
  | UnreliableMessage -- ^ Unrelieable, sequenced but fragments are sent with reliability
  | UnsequencedMessage -- ^ Unreliable and unsequenced (not sort while receiving)
  | UnreliableFragmentedMessage -- ^ Unreliable, sequenced sent with fragments sent within unreliable method
  | UnsequencedFragmentedMessage -- ^ Unreliable, unsequenced with fragments sent within unreliable method
  deriving (Eq, Ord, Bounded, Enum, Show, Generic)

instance NFData MessageType

-- | Payload of event for send errors
data SendError a = SendError {
  sendPeer        :: !(Peer a)
, sendChan        :: !ChannelId
, sendMessageType :: !MessageType
, sendPayload     :: !ByteString
, sendDetails     :: !(BackendSendError a)
} deriving (Generic)

deriving instance HasNetworkBackend a => Show (SendError a)

-- | Holds info about FRP triggers the backend should use and creation options
-- specific for the backend.
data NetworkBackendContext a = NetworkBackendContext {
  -- | Backend specific options (ex. bandwith limits, channel count)
  networkBackendOptions           :: BackendOptions a
  -- | Trigger for event when connection process is finished (succ or fail)
, networkTriggerLocalConnected    :: Peer a -> IO ()
  -- | Trigger event about disconnection of remote peer
, networkTriggerLocalDisonnected  :: IO ()
  -- | Trigger for event when a remote connection is opened
, networkTriggerRemoteConnected   :: Peer a -> IO ()
  -- | Trigger event about disconnection of remote peer
, networkTriggerRemoteDisonnected :: Peer a -> IO ()
  -- | Trigger incoming message event
, networkTriggerIncomingMessage   :: Peer a -> ChannelId -> MessageType -> ByteString -> IO ()
  -- | Trigger event about some async error in backend
, networkTriggerSomeError         :: BackendEventError a -> IO ()
  -- | Trigger event about message sending error
, networkTriggerSendError         :: SendError a -> IO ()
  -- | Trigger event when connection to remote host is failed
, networkTriggerConnectionError   :: (BackendConnectError a, RemoteAddress) -> IO ()
} deriving (Generic)

-- | Holds network backend operations
data NetworkBackend a = NetworkBackend {
  -- | Emmit command to connect to remote endpoint
  networkConnect        :: RemoteAddress -> ConnectOptions a -> IO ()
  -- | Emmit command to disconnect connected peer (or yourself in case of client)
, networkDisconnect     :: Peer a -> IO ()
  -- | Send a message to remote peer.
, networkSendMessage    :: Peer a -> ChannelId -> MessageType -> ByteString -> IO ()
  -- | Shutdown the backend
, networkTerminate      :: IO ()
} deriving (Generic)

-- | Abstract over network backend (ENet or TCP, for instance)
class ( Show (BackendCreateError a)
      , Show (BackendSendError a)
      , Show (BackendConnectError a)
      , Show (BackendEventError a)
      , Show (BackendSendError a)
      , Eq (Peer a)
      , Ord (Peer a)
      , Show (Peer a)
      , Typeable a)
  => HasNetworkBackend a where
  -- | Represents connection to remote machine
  type Peer a :: *
  -- | Represents additional options of backend
  type BackendOptions a :: *
  -- | Represents additional options for connection creation
  type ConnectOptions a :: *

  -- | Type of creation error for the backend
  type BackendCreateError  a :: *
  -- | Type of connection error for the backend
  type BackendConnectError a :: *
  -- | Type of generic event error for the backend
  type BackendEventError   a :: *
  -- | Type of send message error for the backend
  type BackendSendError    a :: *

  -- | Initiate network backend with given parameters and event triggers.
  createNetworkBackend :: MonadIO m => NetworkBackendContext a
    -> m (Either (BackendCreateError a) (NetworkBackend a))
