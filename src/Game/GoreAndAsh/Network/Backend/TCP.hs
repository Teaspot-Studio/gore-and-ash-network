{-|
Module      : Game.GoreAndAsh.Network
Description : Network backend for TCP transport.
Copyright   : (c) Anton Gushcha, 2015-2017
License     : BSD3
Maintainer  : ncrashed@gmail.com
Stability   : experimental
Portability : POSIX
-}
module Game.GoreAndAsh.Network.Backend.TCP(
    TCPBackend
  , TCPPeer
  -- * Options
  , TCPBackendOpts(..)
  , HostName
  , ServiceName
  , TCPParameters(..)
  , defaultTCPParameters
  , ConnectHints(..)
  , defaultConnectHints
  -- * Errors
  , TransportError(..)
  , NewEndPointErrorCode(..)
  , ConnectErrorCode(..)
  , EventErrorCode(..)
  , SendErrorCode(..)
  -- * Utils
  , decodeEndPointAddress
  , encodeEndPointAddress
  , EndPointAddress(..)
  ) where

import Control.Applicative
import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.Extra (whenJust)
import Control.Monad.IO.Class
import Data.ByteString (ByteString)
import Data.IORef
import Data.Map.Strict (Map)
import Data.Maybe
import Data.Monoid
import Data.Sequence (Seq)
import Data.Text (Text)
import Game.GoreAndAsh.Network.Backend
import GHC.Generics
import Network.Socket (ServiceName, HostName)
import Network.Transport
import Network.Transport.TCP
import Network.Transport.TCP.Internal (encodeEndPointAddress, decodeEndPointAddress)

import qualified Control.Immortal as Immortal
import qualified Data.Binary.Get as B
import qualified Data.Binary.Put as B
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map.Strict as M
import qualified Data.Sequence as S
import qualified Data.Text as T

-- | Network backend based on `network-transport-tcp`
data TCPBackend

-- | Specific options of TCP backend
data TCPBackendOpts = TCPBackendOpts {
  tcpHostName    :: HostName -- ^ Server host or ip
, tcpServiceName :: ServiceName -- ^ Server port (usually)
, tcpParameters  :: TCPParameters -- ^ TCP specific options
, tcpDuplexHints :: ConnectHints -- ^ Options for opening reverse direction connections
} deriving (Generic)

-- | Peer record that holds connection to remote server and connection id to identify
-- replies.
data TCPPeer =
  -- | Describes incoming connection
    TCPPeerIncoming {
      peerId         :: ConnectionId
    , peerConnection :: Connection
    , peerAddr       :: EndPointAddress
    }
  -- | Describes outcoming connection
  | TCPPeerOutcoming {
      peerResolvedId :: MVar ConnectionId
    , peerConnection :: Connection
    , peerAddr       :: EndPointAddress
    }
  deriving (Generic)

instance Eq TCPPeer where
  a == b = case a of
    TCPPeerIncoming{} -> case b of
      TCPPeerIncoming{} -> peerId a == peerId b
      _ -> False
    TCPPeerOutcoming{}   -> case b of
      TCPPeerOutcoming{} -> peerAddr a == peerAddr b
      _ -> False

instance Ord TCPPeer where
  compare a b = case a of
    TCPPeerIncoming{} -> case b of
      TCPPeerIncoming{}  -> compare (peerId a) (peerId b)
      TCPPeerOutcoming{} -> compare (peerAddr a) (peerAddr b)
    TCPPeerOutcoming{}   -> compare (peerAddr a) (peerAddr b)

instance Show TCPPeer where
  show TCPPeerIncoming{..}  = "Incoming connection from " <> show peerAddr
  show TCPPeerOutcoming{..} = "Outcoming connection to " <> show peerAddr

data TCPBackendSomeError =
  -- | Error while decoding channel and message length
    TCPChannelFormatError !TCPPeer !Text !ByteString
  -- | Error that came from transport layer
  | TCPEventError (TransportError EventErrorCode)
  deriving (Generic, Show)

instance HasNetworkBackend TCPBackend where
  type Peer TCPBackend = TCPPeer
  type BackendOptions TCPBackend = TCPBackendOpts
  type ConnectOptions TCPBackend = ConnectHints

  type BackendCreateError  TCPBackend = Either IOException (TransportError NewEndPointErrorCode)
  type BackendConnectError TCPBackend = TransportError ConnectErrorCode
  type BackendEventError   TCPBackend = TCPBackendSomeError
  type BackendSendError    TCPBackend = TransportError SendErrorCode

  -- | Initiate network backend with given parameters and event triggers.
  createNetworkBackend ctx = liftIO $ do
    let TCPBackendOpts{..} = networkBackendOptions ctx
    res <- createTransport tcpHostName tcpServiceName (tcpHostName,) tcpParameters
    case res of
      Left er -> return $ Left $ Left er
      Right transport -> do
        epointRes <- newEndPoint transport
        case epointRes of
          Left er -> return $ Left $ Right er
          Right endpoint -> Right <$> makeTCPBackend ctx transport endpoint

-- | Memory storage for backend implementation where some important mappings are stored
data BackendRegistry = BackendRegistry {
  -- | Describes outcoming connections for which we don't know connection id
  serverResolvings :: IORef (Map EndPointAddress (MVar ConnectionId, TCPPeer))
  -- | Descirbes known incomming connections
, serverPeers      :: IORef (Map ConnectionId TCPPeer)
  -- | Holds remainders of read from connection
, serverRemainder  :: IORef (Map ConnectionId ByteString)
}

-- | Create empty registry
newBackendRegistry :: IO BackendRegistry
newBackendRegistry = BackendRegistry
  <$> newIORef mempty
  <*> newIORef mempty
  <*> newIORef mempty

-- | Request remains from previous receive
requestRemains :: BackendRegistry -> ConnectionId -> IO ByteString
requestRemains BackendRegistry{..} cid = do
  m <- readIORef serverRemainder
  return $ fromMaybe mempty $ M.lookup cid m

-- | Store remains from read for peer
putRemains :: BackendRegistry -> ConnectionId -> ByteString -> IO ()
putRemains BackendRegistry{..} cid bs =
  atomicModifyIORef' serverRemainder $ \m -> (M.insert cid bs m, ())

-- | If the given id is an id that is being resolved for outcoming connection, resolve it
-- and return 'True', else it is incoming connection and the function returns 'False'
checkOutcomingResolve :: BackendRegistry -> ConnectionId -> EndPointAddress -> IO Bool
checkOutcomingResolve registry@BackendRegistry{..} cid addr = do
  mdatum <- atomicModifyIORef' serverResolvings $ \m -> case M.lookup addr m of
    Nothing   -> (m, Nothing)
    Just datum -> (M.delete addr m, Just datum)
  case mdatum of
    Nothing -> return False
    Just (mvar, peer) -> do
      registerConnection registry cid peer
      putMVar mvar cid
      return True

-- | Find a peer that is registered with given connection id. Blocks if the id
-- within a resolving peer until it is resolved.
registryFindPeer :: BackendRegistry -> ConnectionId -> IO TCPPeer
registryFindPeer registry@BackendRegistry{..} cid = do
  peers <- readIORef serverPeers
  case M.lookup cid peers of
    Just peer -> return peer
    Nothing -> do
      threadDelay 100000
      registryFindPeer registry cid

-- | Register peer under given connection id
registerConnection :: BackendRegistry -> ConnectionId -> TCPPeer -> IO ()
registerConnection BackendRegistry{..} cid peer =
  atomicModifyIORef' serverPeers $ \m -> (M.insert cid peer m, ())

-- | Register new resolving outcoming connection, resolves are finished when server
-- opens full duplex connection.
registerResolving :: BackendRegistry -> EndPointAddress -> MVar ConnectionId -> TCPPeer -> IO ()
registerResolving BackendRegistry{..} addr mvar peer =
  atomicModifyIORef' serverResolvings $ \m -> (M.insert addr (mvar, peer) m, ())

-- | Forget about given connection id
removeConnection :: BackendRegistry -> ConnectionId -> IO (Maybe TCPPeer)
removeConnection BackendRegistry{..} cid = do
  m <- atomicModifyIORef' serverPeers $ \m -> (M.delete cid m, m)
  return $ M.lookup cid m

-- | Remove partial connections for given server address
removeResolving :: BackendRegistry -> EndPointAddress -> IO ()
removeResolving BackendRegistry{..} addr =
  atomicModifyIORef' serverResolvings $ \m -> (M.delete addr m, ())

-- | Find connections by connection address
findPeersByAddr :: BackendRegistry -> EndPointAddress -> IO [TCPPeer]
findPeersByAddr BackendRegistry{..} addr = do
  mpeers <- readIORef serverPeers
  let peers = filter ((addr ==) . peerAddr) . M.elems $ mpeers
  resolves <- readIORef serverResolvings
  let rpeers = maybe [] (pure . snd) $ M.lookup addr resolves
  return $ rpeers ++ peers

-- | Create TCP backend from given transport and enpoint (and triggers set)
makeTCPBackend :: NetworkBackendContext TCPBackend
  -> Transport
  -> EndPoint
  -> IO (NetworkBackend TCPBackend)
makeTCPBackend NetworkBackendContext{..} transport endpoint = do
  registry <- newBackendRegistry
  -- start listen thread
  _ <- Immortal.createWithLabel "tcpBackend" $ \immortal -> forever $ do
    e <- receive endpoint
    case e of
      Received cid bss -> do
        bs <- requestRemains registry cid
        let fullBs = mconcat (bs : bss)
            emsgs = decodeChannelMsgs fullBs
            remains = case emsgs of
              Left _ -> mempty
              Right (_, remainBs) -> remainBs
        putRemains registry cid remains
        void $ forkIO $ do
          peer <- registryFindPeer registry cid -- blocking in case of TCPPeerOutcoming
          case emsgs of
            Left er -> networkTriggerSomeError $ TCPChannelFormatError peer er fullBs
            Right (msgs, _) -> forM_ msgs $ \(chan, msg) ->
              networkTriggerIncomingMessage peer chan ReliableMessage msg
      ConnectionClosed cid -> void $ forkIO $ do
        mpeer <- removeConnection registry cid
        whenJust mpeer networkTriggerRemoteDisonnected
      ConnectionOpened cid _ addr -> void $ forkIO $ do
        isOucoming <- checkOutcomingResolve registry cid addr
        if isOucoming then return ()
          else do
            res <- connect endpoint addr ReliableOrdered (tcpDuplexHints networkBackendOptions)
            case res of
              Left er -> do
                let EndPointAddress addr' = addr
                networkTriggerConnectionError (er, addr')
              Right con -> do
                let peer = TCPPeerIncoming {
                    peerId         = cid
                  , peerConnection = con
                  , peerAddr       = addr
                  }
                registerConnection registry cid peer
                networkTriggerRemoteConnected peer
      ReceivedMulticast _ _ -> return () -- TODO: support this
      EndPointClosed -> Immortal.stop immortal
      ErrorEvent er -> do
        void $ forkIO $ case er of
          TransportError (EventConnectionLost addr) _ -> do
            peers <- findPeersByAddr registry addr
            forM_ peers $ \peer -> case peer of
              TCPPeerIncoming{} -> do
                mpeer <- removeConnection registry $ peerId peer
                whenJust mpeer networkTriggerRemoteDisonnected
              TCPPeerOutcoming{} -> do
                removeResolving registry addr
                let er' = TransportError ConnectFailed "Lost connection"
                    EndPointAddress addr' = addr
                networkTriggerConnectionError (er', addr')
          _ -> return ()
        networkTriggerSomeError $ TCPEventError er

  -- Define backend operations
  let
    networkConnect addr hints = void $ forkIO $ do
      let addr' = EndPointAddress addr
      res <- connect endpoint addr' ReliableOrdered hints
      var <- newEmptyMVar
      case res of
        Left er -> networkTriggerConnectionError (er, addr)
        Right con -> do
          let peer = TCPPeerOutcoming {
                  peerResolvedId = var
                , peerConnection = con
                , peerAddr = addr'
                }
          registerResolving registry addr' var peer
          networkTriggerLocalConnected peer
    networkDisconnect peer = void $ forkIO $ do
      close $ peerConnection peer
      networkTriggerLocalDisonnected
    networkSendMessage peer chan mt bs = void $ forkIO $ do
      let msg = encodeChannelMsg chan bs
      res <- send (peerConnection peer) [msg]
      case res of
        Left er -> networkTriggerSendError $ SendError peer chan mt bs er
        Right _ -> return ()
    networkTerminate = closeTransport transport
  return NetworkBackend {..}

-- | Parse datagrams from stream of bytes, return result and read remainder
decodeChannelMsgs :: ByteString -> Either Text (Seq (ChannelId, ByteString), ByteString)
decodeChannelMsgs bs
  | BS.length bs < 8 = Right (mempty, bs) -- too short
  | otherwise = case B.runGetOrFail (msgsDecoder mempty) $ BSL.fromStrict bs of
    Left  (_, _, e)    -> Left $ T.pack e
    Right (_, i, msgs) -> Right (msgs, BS.drop (fromIntegral i) bs)
  where
    msgsDecoder !msgs = do
      mmsg <- msgDecoder
      case mmsg of
        Nothing -> return msgs
        Just msg -> do
          let msgs' = msgs S.|> msg
          n <- B.bytesRead
          if fromIntegral n + 8 > BS.length bs
            then return msgs'
            else msgsDecoder msgs'

    msgDecoder = B.lookAheadM $ do
      l <- B.getWord32be
      ch <- B.getWord32be
      fmap (Just . (ChannelId ch,)) (B.getByteString $ fromIntegral l) <|> pure Nothing

-- | Encode message that we can reconstruct it from stream of bytes
encodeChannelMsg :: ChannelId -> ByteString -> ByteString
encodeChannelMsg chan msg = BSL.toStrict . B.runPut $ do
  B.putWord32be $ fromIntegral $ BS.length msg
  B.putWord32be $ fromIntegral $ unChannelId chan
  B.putByteString msg
