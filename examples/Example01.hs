module Main where

import Data.Monoid
import Game.GoreAndAsh
import Game.GoreAndAsh.Time
import Game.GoreAndAsh.Logging
import Game.GoreAndAsh.Network
import Game.GoreAndAsh.Network.Backend.TCP
import System.Environment

-- | Application monad that is used for implementation of game API
type AppMonad = NetworkT Spider TCPBackend (LoggingT Spider GMSpider)

-- Server application.
-- The application should be generic in the host monad that is used
appServer :: forall t m backend . (LoggingMonad t m, NetworkServer t backend m) => ServiceName -> m ()
appServer p = do
  loggingSetDebugFlag True
  e <- getPostBuild
  logInfoE $ ffor e $ const $ "Started to listen port " <> showl p <> " ..."

  connE <- peerConnected
  logInfoE $ ffor connE $ const $ "Peer is connected..."

  discE <- peerDisconnected
  logInfoE $ ffor discE $ const $ "Peer is disconnected..."

  someErrorE <- networkSomeError
  sendErrorE <- networkSendError
  logWarnE $ ffor someErrorE $ \er -> "Network error: " <> showl er
  logWarnE $ ffor sendErrorE $ \er -> "Network send error: " <> showl er

  _ <- processPeers peerWidget
  return ()
  where
  peerWidget :: Peer backend -> m ()
  peerWidget peer = do
    let chan = ChannelId 0
    msgE <- peerChanMessage peer chan
    logInfoE $ ffor msgE $ \msg -> "Peer send a message: " <> showl msg
    _ <- peerChanSend peer chan ((ReliableMessage,) <$> msgE)
    return ()

-- Client application.
-- The application should be generic in the host monad that is used
appClient :: (LoggingMonad t m, NetworkClient t TCPBackend m) => HostName -> ServiceName -> m ()
appClient host serv = do
  e <- getPostBuild
  let EndPointAddress addr = encodeEndPointAddress host serv 0
  connectedE <- clientConnect $ ffor e $ const (addr, defaultConnectHints)
  conErrorE <- networkConnectionError
  logInfoE $ ffor connectedE $ const "Connected to server!"
  logErrorE $ ffor conErrorE $ \er -> "Failed to connect: " <> showl er

  _ <- whenConnected (pure ()) $ \server -> do
    buildE <- getPostBuild
    tickE <- tickEvery (realToFrac (1 :: Double))
    let sendE = leftmost [tickE, buildE]
        chan = ChannelId 0
    _ <- peerChanSend server chan $ ffor sendE $ const (ReliableMessage, "Hello, server!")
    respondE <- peerChanMessage server chan
    logInfoE $ ffor respondE $ \msg -> "Server respond: " <> showl msg
  return ()

data Mode = Client HostName ServiceName | Server ServiceName

readArgs :: IO Mode
readArgs = do
  args <- getArgs
  case args of
    ["client", host, serv] -> return $ Client host serv
    ["server", p] -> return $ Server p
    _ -> fail $ "Expected arguments: client <host> <port> | server <port>"

main :: IO ()
main = do
  mode <- readArgs
  let app :: AppMonad ()
      app = case mode of
        Client host serv -> appClient host serv
        Server port -> appServer port
      tcpOpts = TCPBackendOpts {
          tcpHostName = "127.0.0.1"
        , tcpServiceName = case mode of
             Client _ _ -> ""
             Server port -> port
        , tcpParameters = defaultTCPParameters
        , tcpDuplexHints = defaultConnectHints
        }
      opts = (defaultNetworkOptions tcpOpts) { networkOptsDetailedLogging = True }
  mres <- runGM $ runLoggerT $ runNetworkT opts (app :: AppMonad ())
  case mres of
    Left er -> print $ renderNetworkError er
    Right _ -> pure ()
