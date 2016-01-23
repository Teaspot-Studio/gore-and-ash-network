module Network.ENet.Internal where

import Network.Socket.Internal

import Foreign

import Data.Endian
import Data.Endian.Unsafe( unsafeUnwrapBigEndian
                         , unsafeAssertBigEndian)

import Network.ENet.Bindings

toENetAddress :: SockAddr -> Address
toENetAddress (SockAddrInet port ip) =
  Address ip $ fromBigEndian $ unsafeAssertBigEndian $ fromIntegral port
toENetAddress _ = error "Unsupported enet address!"

toSockAddr :: Address -> SockAddr
toSockAddr (Address ip port) =
  SockAddrInet (fromIntegral $ unsafeUnwrapBigEndian $ toBigEndian $ port) ip

withMaybeDo :: Storable a => Maybe a -> (Ptr a -> IO b) -> IO b
withMaybeDo (Just val) f = alloca $ \ptr -> poke ptr val >> f ptr
withMaybeDo Nothing    f = f nullPtr
