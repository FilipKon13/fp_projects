{-# LINE 1 "Network/Socket/Posix/Cmsg.hsc" #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Network.Socket.Posix.Cmsg where






import Data.ByteString.Internal
import Foreign.ForeignPtr
import Foreign.Marshal.Array (peekArray, pokeArray)
import System.IO.Unsafe (unsafeDupablePerformIO)
import System.Posix.Types (Fd(..))

import Network.Socket.Imports
import Network.Socket.Types
import Network.Socket.ReadShow

import qualified Text.Read as P

-- | Control message (ancillary data) including a pair of level and type.
data Cmsg = Cmsg {
    cmsgId   :: CmsgId
  , cmsgData :: ByteString
  } deriving (Eq, Show)

----------------------------------------------------------------

-- | Identifier of control message (ancillary data).
data CmsgId = CmsgId {
    cmsgLevel :: CInt
  , cmsgType  :: CInt
  } deriving (Eq)

-- | Unsupported identifier
pattern UnsupportedCmsgId :: CmsgId
pattern UnsupportedCmsgId = CmsgId (-1) (-1)

-- | The identifier for 'IPv4TTL'.
pattern CmsgIdIPv4TTL :: CmsgId

{-# LINE 52 "Network/Socket/Posix/Cmsg.hsc" #-}
pattern CmsgIdIPv4TTL = CmsgId (0) (2)
{-# LINE 53 "Network/Socket/Posix/Cmsg.hsc" #-}

{-# LINE 54 "Network/Socket/Posix/Cmsg.hsc" #-}

-- | The identifier for 'IPv6HopLimit'.
pattern CmsgIdIPv6HopLimit :: CmsgId
pattern CmsgIdIPv6HopLimit = CmsgId (41) (52)
{-# LINE 58 "Network/Socket/Posix/Cmsg.hsc" #-}

-- | The identifier for 'IPv4TOS'.
pattern CmsgIdIPv4TOS :: CmsgId

{-# LINE 64 "Network/Socket/Posix/Cmsg.hsc" #-}
pattern CmsgIdIPv4TOS = CmsgId (0) (1)
{-# LINE 65 "Network/Socket/Posix/Cmsg.hsc" #-}

{-# LINE 66 "Network/Socket/Posix/Cmsg.hsc" #-}

-- | The identifier for 'IPv6TClass'.
pattern CmsgIdIPv6TClass :: CmsgId
pattern CmsgIdIPv6TClass = CmsgId (41) (67)
{-# LINE 70 "Network/Socket/Posix/Cmsg.hsc" #-}

-- | The identifier for 'IPv4PktInfo'.
pattern CmsgIdIPv4PktInfo :: CmsgId

{-# LINE 74 "Network/Socket/Posix/Cmsg.hsc" #-}
pattern CmsgIdIPv4PktInfo = CmsgId (0) (8)
{-# LINE 75 "Network/Socket/Posix/Cmsg.hsc" #-}

{-# LINE 78 "Network/Socket/Posix/Cmsg.hsc" #-}

-- | The identifier for 'IPv6PktInfo'.
pattern CmsgIdIPv6PktInfo :: CmsgId

{-# LINE 82 "Network/Socket/Posix/Cmsg.hsc" #-}
pattern CmsgIdIPv6PktInfo = CmsgId (41) (50)
{-# LINE 83 "Network/Socket/Posix/Cmsg.hsc" #-}

{-# LINE 86 "Network/Socket/Posix/Cmsg.hsc" #-}

-- | The identifier for 'Fds'.
pattern CmsgIdFds :: CmsgId
pattern CmsgIdFds = CmsgId (1) (1)
{-# LINE 90 "Network/Socket/Posix/Cmsg.hsc" #-}

----------------------------------------------------------------

-- | Locate a control message of the given type in a list of control
--   messages. The following shows an example usage:
--
-- > (lookupCmsg CmsgIdIPv4TOS cmsgs >>= decodeCmsg) :: Maybe IPv4TOS
lookupCmsg :: CmsgId -> [Cmsg] -> Maybe Cmsg
lookupCmsg cid cmsgs = find (\cmsg -> cmsgId cmsg == cid) cmsgs

-- | Filtering control message.
filterCmsg :: CmsgId -> [Cmsg] -> [Cmsg]
filterCmsg cid cmsgs = filter (\cmsg -> cmsgId cmsg == cid) cmsgs

----------------------------------------------------------------

-- | Control message type class.
--   Each control message type has a numeric 'CmsgId' and encode
--   and decode functions.
class ControlMessage a where
    controlMessageId :: CmsgId
    encodeCmsg :: a -> Cmsg
    decodeCmsg :: Cmsg -> Maybe a

encodeStorableCmsg :: forall a . (ControlMessage a, Storable a) => a -> Cmsg
encodeStorableCmsg x = unsafeDupablePerformIO $ do
    bs <- create siz $ \p0 -> do
        let p = castPtr p0
        poke p x
    let cmsid = controlMessageId @a
    return $ Cmsg cmsid bs
  where
    siz = sizeOf x

decodeStorableCmsg :: forall a . (ControlMessage a, Storable a) => Cmsg -> Maybe a
decodeStorableCmsg (Cmsg cmsid (PS fptr off len))
  | cid /= cmsid = Nothing
  | len < siz    = Nothing
  | otherwise    = unsafeDupablePerformIO $ withForeignPtr fptr $ \p0 -> do
        let p = castPtr (p0 `plusPtr` off)
        Just <$> peek p
  where
    cid = controlMessageId @a
    siz = sizeOf (undefined :: a)

----------------------------------------------------------------

-- | Time to live of IPv4.

{-# LINE 141 "Network/Socket/Posix/Cmsg.hsc" #-}
newtype IPv4TTL = IPv4TTL CInt deriving (Eq, Show, Storable)

{-# LINE 143 "Network/Socket/Posix/Cmsg.hsc" #-}

instance ControlMessage IPv4TTL where
    controlMessageId = CmsgIdIPv4TTL
    encodeCmsg = encodeStorableCmsg
    decodeCmsg = decodeStorableCmsg

----------------------------------------------------------------

-- | Hop limit of IPv6.
newtype IPv6HopLimit = IPv6HopLimit CInt deriving (Eq, Show, Storable)

instance ControlMessage IPv6HopLimit where
    controlMessageId = CmsgIdIPv6HopLimit
    encodeCmsg = encodeStorableCmsg
    decodeCmsg = decodeStorableCmsg

----------------------------------------------------------------

-- | TOS of IPv4.
newtype IPv4TOS = IPv4TOS CChar deriving (Eq, Show, Storable)

instance ControlMessage IPv4TOS where
    controlMessageId = CmsgIdIPv4TOS
    encodeCmsg = encodeStorableCmsg
    decodeCmsg = decodeStorableCmsg

----------------------------------------------------------------

-- | Traffic class of IPv6.
newtype IPv6TClass = IPv6TClass CInt deriving (Eq, Show, Storable)

instance ControlMessage IPv6TClass where
    controlMessageId = CmsgIdIPv6TClass
    encodeCmsg = encodeStorableCmsg
    decodeCmsg = decodeStorableCmsg

----------------------------------------------------------------

-- | Network interface ID and local IPv4 address.
data IPv4PktInfo = IPv4PktInfo Int HostAddress HostAddress deriving (Eq)

instance Show IPv4PktInfo where
    show (IPv4PktInfo n sa ha) = "IPv4PktInfo " ++ show n ++ " " ++ show (hostAddressToTuple sa) ++ " " ++ show (hostAddressToTuple ha)

instance ControlMessage IPv4PktInfo where
    controlMessageId = CmsgIdIPv4PktInfo
    encodeCmsg = encodeStorableCmsg
    decodeCmsg = decodeStorableCmsg

instance Storable IPv4PktInfo where

{-# LINE 194 "Network/Socket/Posix/Cmsg.hsc" #-}
    sizeOf    ~_ = ((12))
{-# LINE 195 "Network/Socket/Posix/Cmsg.hsc" #-}
    alignment ~_ = alignment (0 :: CInt)
    poke p (IPv4PktInfo n sa ha) = do
        ((\hsc_ptr -> pokeByteOff hsc_ptr 0))  p (fromIntegral n :: CInt)
{-# LINE 198 "Network/Socket/Posix/Cmsg.hsc" #-}
        ((\hsc_ptr -> pokeByteOff hsc_ptr 4)) p sa
{-# LINE 199 "Network/Socket/Posix/Cmsg.hsc" #-}
        ((\hsc_ptr -> pokeByteOff hsc_ptr 8))     p ha
{-# LINE 200 "Network/Socket/Posix/Cmsg.hsc" #-}
    peek p = do
        n  <- ((\hsc_ptr -> peekByteOff hsc_ptr 0))  p
{-# LINE 202 "Network/Socket/Posix/Cmsg.hsc" #-}
        sa <- ((\hsc_ptr -> peekByteOff hsc_ptr 4)) p
{-# LINE 203 "Network/Socket/Posix/Cmsg.hsc" #-}
        ha <- ((\hsc_ptr -> peekByteOff hsc_ptr 8))     p
{-# LINE 204 "Network/Socket/Posix/Cmsg.hsc" #-}
        return $ IPv4PktInfo n sa ha

{-# LINE 211 "Network/Socket/Posix/Cmsg.hsc" #-}

----------------------------------------------------------------

-- | Network interface ID and local IPv4 address.
data IPv6PktInfo = IPv6PktInfo Int HostAddress6 deriving (Eq)

instance Show IPv6PktInfo where
    show (IPv6PktInfo n ha6) = "IPv6PktInfo " ++ show n ++ " " ++ show (hostAddress6ToTuple ha6)

instance ControlMessage IPv6PktInfo where
    controlMessageId = CmsgIdIPv6PktInfo
    encodeCmsg = encodeStorableCmsg
    decodeCmsg = decodeStorableCmsg

instance Storable IPv6PktInfo where

{-# LINE 227 "Network/Socket/Posix/Cmsg.hsc" #-}
    sizeOf    ~_ = ((20))
{-# LINE 228 "Network/Socket/Posix/Cmsg.hsc" #-}
    alignment ~_ = alignment (0 :: CInt)
    poke p (IPv6PktInfo n ha6) = do
        ((\hsc_ptr -> pokeByteOff hsc_ptr 16)) p (fromIntegral n :: CInt)
{-# LINE 231 "Network/Socket/Posix/Cmsg.hsc" #-}
        ((\hsc_ptr -> pokeByteOff hsc_ptr 0))    p (In6Addr ha6)
{-# LINE 232 "Network/Socket/Posix/Cmsg.hsc" #-}
    peek p = do
        In6Addr ha6 <- ((\hsc_ptr -> peekByteOff hsc_ptr 0))    p
{-# LINE 234 "Network/Socket/Posix/Cmsg.hsc" #-}
        n :: CInt   <- ((\hsc_ptr -> peekByteOff hsc_ptr 16)) p
{-# LINE 235 "Network/Socket/Posix/Cmsg.hsc" #-}
        return $ IPv6PktInfo (fromIntegral n) ha6

{-# LINE 242 "Network/Socket/Posix/Cmsg.hsc" #-}

----------------------------------------------------------------

instance ControlMessage [Fd] where
    controlMessageId = CmsgIdFds

    encodeCmsg fds = unsafeDupablePerformIO $ do
        bs <- create siz $ \p0 -> do
            let p = castPtr p0
            pokeArray p fds
        return $ Cmsg CmsgIdFds bs
        where
            siz = sizeOf (undefined :: Fd) * length fds

    decodeCmsg (Cmsg cmsid (PS fptr off len))
        | cmsid /= CmsgIdFds = Nothing
        | otherwise          =
            unsafeDupablePerformIO $ withForeignPtr fptr $ \p0 -> do
                let p = castPtr (p0 `plusPtr` off)
                    numFds = len `div` sizeOfFd
                Just <$> peekArray numFds p
        where
            sizeOfFd = sizeOf (undefined :: Fd)

cmsgIdBijection :: Bijection CmsgId String
cmsgIdBijection =
    [ (UnsupportedCmsgId, "UnsupportedCmsgId")
    , (CmsgIdIPv4TTL, "CmsgIdIPv4TTL")
    , (CmsgIdIPv6HopLimit, "CmsgIdIPv6HopLimit")
    , (CmsgIdIPv4TOS, "CmsgIdIPv4TOS")
    , (CmsgIdIPv6TClass, "CmsgIdIPv6TClass")
    , (CmsgIdIPv4PktInfo, "CmsgIdIPv4PktInfo")
    , (CmsgIdIPv6PktInfo, "CmsgIdIPv6PktInfo")
    , (CmsgIdFds, "CmsgIdFds")
    ]

instance Show CmsgId where
    showsPrec = bijectiveShow cmsgIdBijection def
      where
        defname = "CmsgId"
        unId = \(CmsgId l t) -> (l,t)
        def = defShow defname unId showIntInt

instance Read CmsgId where
    readPrec = bijectiveRead cmsgIdBijection def
      where
        defname = "CmsgId"
        def = defRead defname (uncurry CmsgId) readIntInt
