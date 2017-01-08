{-|
Module      : Game.GoreAndAsh.Network
Description : Network message definition
Copyright   : (c) Anton Gushcha, 2015-2016
License     : BSD3
Maintainer  : ncrashed@gmail.com
Stability   : experimental
Portability : POSIX
-}
module Game.GoreAndAsh.Network.Message(
    Peer
  , Message(..)
  , MessageType(..)
  , NetworkMessage(..)
  , messageToPacket
  , bitsToMessageType
  ) where

import Control.DeepSeq
import Foreign
import GHC.Generics
import Network.ENet

import qualified Data.ByteString as BS
import qualified Network.ENet.Bindings as B

-- | Strategy how given message is delivered to remote host
data MessageType =
    ReliableMessage -- ^ TCP like, ordered reliable delivery
  | UnreliableMessage -- ^ Unrelieable, sequenced but fragments are sent with reliability
  | UnsequencedMessage -- ^ Unreliable and unsequenced (not sort while receiving)
  | UnreliableFragmentedMessage -- ^ Unreliable, sequenced sent with fragments sent within unreliable method
  | UnsequencedFragmentedMessage -- ^ Unreliable, unsequenced with fragments sent within unreliable method
  deriving (Eq, Ord, Bounded, Enum, Show, Generic)

instance NFData MessageType

-- | Remote endpoint
type Peer = Ptr B.Peer

-- | High-level contents of network message
data NetworkMessage = NetworkMessage {
  networkMsgPeer    :: !Peer -- ^ Peer the source of the message
, networkMsgChan    :: !B.ChannelID -- ^ Channel the message came from
, networkMsgType    :: !MessageType -- ^ Reliability of message
, networkMsgPayload :: !BS.ByteString -- ^ Payload of message
} deriving (Generic, Show)


-- | Converts high-level message type to bits option for ENet
messageTypeToBits :: MessageType -> PacketFlagSet
messageTypeToBits t = case t of
  ReliableMessage -> makePacketFlagSet [B.Reliable]
  UnsequencedMessage -> makePacketFlagSet [B.Unsequenced]
  UnsequencedFragmentedMessage -> makePacketFlagSet [B.UnreliableFragment, B.Unsequenced]
  UnreliableMessage -> emptyPacketFlagSet
  UnreliableFragmentedMessage -> makePacketFlagSet [B.UnreliableFragment]

-- | Converts low-level message flags to high-level message type
bitsToMessageType :: PacketFlagSet -> MessageType
bitsToMessageType fs' = if B.Reliable `elem` fs then ReliableMessage
  else if B.UnreliableFragment `elem` fs then
    if B.Unsequenced `elem` fs then UnsequencedFragmentedMessage
    else UnreliableFragmentedMessage
  else if B.Unsequenced `elem` fs then UnsequencedMessage
    else UnreliableMessage
  where
    fs = unpackPacketFlagSet fs'

-- | Message that has individual options about reliability
data Message = Message {
  messageType :: !MessageType
, messagePayload :: !BS.ByteString
} deriving (Show, Generic)

instance NFData Message

-- | Convert message to internal ENet packet
messageToPacket :: Message -> Packet
messageToPacket (Message mt p) = Packet (messageTypeToBits mt) p