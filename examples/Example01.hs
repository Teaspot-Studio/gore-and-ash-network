module Main where

import Control.Monad.IO.Class
import Data.Monoid
import Game.GoreAndAsh
import Game.GoreAndAsh.Time
import Game.GoreAndAsh.Logging
import Game.GoreAndAsh.Network
import Network.Socket
import System.Environment
import Text.Read

-- | Application monad that is used for implementation of game API
type AppMonad = TimerT Spider (NetworkT Spider (LoggingT Spider(GameMonad Spider)))

-- Server application.
-- The application should be generic in the host monad that is used
appServer :: (LoggingMonad t m, NetworkServer t m) => PortNumber -> m ()
appServer p = do
  e <- getPostBuild
  loggingSetDebugFlag True
  listenE <- dontCare =<< (serverListen $ ffor e $ const $ ServerListen {
      listenAddress = SockAddrInet p 0
    , listenMaxConns = 100
    , listenChanns = 2
    , listenIncoming = 0
    , listenOutcoming = 0
    })
  logInfoE $ ffor listenE $ const $ "Started to listen port " <> showl p <> " ..."

  connE <- peerConnected
  logInfoE $ ffor connE $ const $ "Peer is connected..."

  discE <- peerDisconnected
  logInfoE $ ffor discE $ const $ "Peer is disconnected..."

  _ <- processPeers peerWidget
  return ()
  where
  peerWidget peer = do
    let chan = mempty
    msgsE <- peerChanMessages peer chan
    logInfoE $ ffor msgsE $ \msgs -> "Peer send messages: " <> showl (fmap networkMsgPayload msgs)
    peerChanSendMany peer chan (fmap (Message ReliableMessage . networkMsgPayload) <$> msgsE)

-- | Find server address by host name or IP
resolveServer :: MonadIO m => HostName -> ServiceName -> m SockAddr
resolveServer host serv = do
  info <- liftIO $ getAddrInfo Nothing (Just host) (Just serv)
  case info of
    [] -> fail $ "Cannot resolve server address: " <> host
    (a : _) -> return $ addrAddress a

-- Client application.
-- The application should be generic in the host monad that is used
appClient :: (LoggingMonad t m, TimerMonad t m, NetworkClient t m) => HostName -> ServiceName -> m ()
appClient host serv = do
  addr <- resolveServer host serv
  e <- getPostBuild
  connectedE <- dontCare =<< (clientConnect $ ffor e $ const $ ClientConnect {
      clientAddrr = addr
    , clientChanns = 2
    , clientIncoming = 0
    , clientOutcoming = 0
    })
  logInfoE $ ffor connectedE $ const "Connected to server!"
  _ <- whenConnected (pure ()) $ \server -> do
    buildE <- getPostBuild
    tickE <- tickEvery (realToFrac (1 :: Double))
    let sendE = leftmost [tickE, buildE]
    _ <- peerChanSend server mempty $ ffor sendE $ const $ Message ReliableMessage "Hello, server!"
    respondsE <- peerChanMessages server mempty
    logInfoE $ ffor respondsE $ \msgs -> "Server responds: " <> showl (fmap networkMsgPayload msgs)
  return ()

data Mode = Client HostName ServiceName | Server PortNumber

readArgs :: IO Mode
readArgs = do
  args <- getArgs
  case args of
    ["client", host, serv] -> return $ Client host serv
    ["server", ps] -> case readMaybe ps of
      Nothing -> fail $ "Failed to parse port!"
      Just p -> return $ Server p
    _ -> fail $ "Expected arguments: client <host> <port> | server <port>"

main :: IO ()
main = do
  mode <- readArgs
  let app :: AppMonad ()
      app = case mode of
        Client host serv -> appClient host serv
        Server port -> appServer port
      opts = (defaultNetworkOptions ()) { networkDetailedLogging = True }
  runSpiderHost $ hostApp $ runModule opts (app :: AppMonad ())