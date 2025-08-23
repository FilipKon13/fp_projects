{-# LINE 1 "Network/Socket/Info.hsc" #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}


#include "HsNetDef.h"

module Network.Socket.Info where

import Control.Exception (mask, onException)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Foreign.Marshal.Alloc (alloca, allocaBytes)
import Foreign.Marshal.Utils (maybeWith, with)
import GHC.IO.Exception (IOErrorType(NoSuchThing))
import System.IO.Error (ioeSetErrorString, mkIOError)

import Network.Socket.Imports
import Network.Socket.Internal
import Network.Socket.Syscall
import Network.Socket.Types

-----------------------------------------------------------------------------

-- | Either a host name e.g., @\"haskell.org\"@ or a numeric host
-- address string consisting of a dotted decimal IPv4 address or an
-- IPv6 address e.g., @\"192.168.0.1\"@.
type HostName       = String
-- | Either a service name e.g., @\"http\"@ or a numeric port number.
type ServiceName    = String

-----------------------------------------------------------------------------
-- Address and service lookups

-- | Flags that control the querying behaviour of 'getAddrInfo'.
--   For more information, see <https://tools.ietf.org/html/rfc3493#page-25>
data AddrInfoFlag =
    -- | The list of returned 'AddrInfo' values will
    --   only contain IPv4 addresses if the local system has at least
    --   one IPv4 interface configured, and likewise for IPv6.
    --   (Only some platforms support this.)
      AI_ADDRCONFIG
    -- | If 'AI_ALL' is specified, return all matching IPv6 and
    --   IPv4 addresses.  Otherwise, this flag has no effect.
    --   (Only some platforms support this.)
    | AI_ALL
    -- | The 'addrCanonName' field of the first returned
    --   'AddrInfo' will contain the "canonical name" of the host.
    | AI_CANONNAME
    -- | The 'HostName' argument /must/ be a numeric
    --   address in string form, and network name lookups will not be
    --   attempted.
    | AI_NUMERICHOST
    -- | The 'ServiceName' argument /must/ be a port
    --   number in string form, and service name lookups will not be
    --   attempted. (Only some platforms support this.)
    | AI_NUMERICSERV
    -- | If no 'HostName' value is provided, the network
    --   address in each 'SockAddr'
    --   will be left as a "wild card".
    --   This is useful for server applications that
    --   will accept connections from any client.
    | AI_PASSIVE
    -- | If an IPv6 lookup is performed, and no IPv6
    --   addresses are found, IPv6-mapped IPv4 addresses will be
    --   returned. (Only some platforms support this.)
    | AI_V4MAPPED
    deriving (Eq, Read, Show)

aiFlagMapping :: [(AddrInfoFlag, CInt)]

aiFlagMapping =
    [

{-# LINE 75 "Network/Socket/Info.hsc" #-}
     (AI_ADDRCONFIG, 32),
{-# LINE 76 "Network/Socket/Info.hsc" #-}

{-# LINE 79 "Network/Socket/Info.hsc" #-}

{-# LINE 80 "Network/Socket/Info.hsc" #-}
     (AI_ALL, 16),
{-# LINE 81 "Network/Socket/Info.hsc" #-}

{-# LINE 84 "Network/Socket/Info.hsc" #-}
     (AI_CANONNAME, 2),
{-# LINE 85 "Network/Socket/Info.hsc" #-}
     (AI_NUMERICHOST, 4),
{-# LINE 86 "Network/Socket/Info.hsc" #-}

{-# LINE 87 "Network/Socket/Info.hsc" #-}
     (AI_NUMERICSERV, 1024),
{-# LINE 88 "Network/Socket/Info.hsc" #-}

{-# LINE 91 "Network/Socket/Info.hsc" #-}
     (AI_PASSIVE, 1),
{-# LINE 92 "Network/Socket/Info.hsc" #-}

{-# LINE 93 "Network/Socket/Info.hsc" #-}
     (AI_V4MAPPED, 8)
{-# LINE 94 "Network/Socket/Info.hsc" #-}

{-# LINE 97 "Network/Socket/Info.hsc" #-}
    ]

-- | Indicate whether the given 'AddrInfoFlag' will have any effect on
-- this system.
addrInfoFlagImplemented :: AddrInfoFlag -> Bool
addrInfoFlagImplemented f = packBits aiFlagMapping [f] /= 0

data AddrInfo = AddrInfo {
    addrFlags :: [AddrInfoFlag]
  , addrFamily :: Family
  , addrSocketType :: SocketType
  , addrProtocol :: ProtocolNumber
  , addrAddress :: SockAddr
  , addrCanonName :: Maybe String
  } deriving (Eq, Show)

instance Storable AddrInfo where
    sizeOf    ~_ = 48
{-# LINE 115 "Network/Socket/Info.hsc" #-}
    alignment ~_ = alignment (0 :: CInt)

    peek p = do
        ai_flags <- ((\hsc_ptr -> peekByteOff hsc_ptr 0)) p
{-# LINE 119 "Network/Socket/Info.hsc" #-}
        ai_family <- ((\hsc_ptr -> peekByteOff hsc_ptr 4)) p
{-# LINE 120 "Network/Socket/Info.hsc" #-}
        ai_socktype <- ((\hsc_ptr -> peekByteOff hsc_ptr 8)) p
{-# LINE 121 "Network/Socket/Info.hsc" #-}
        ai_protocol <- ((\hsc_ptr -> peekByteOff hsc_ptr 12)) p
{-# LINE 122 "Network/Socket/Info.hsc" #-}
        ai_addr <- ((\hsc_ptr -> peekByteOff hsc_ptr 24)) p >>= peekSockAddr
{-# LINE 123 "Network/Socket/Info.hsc" #-}
        ai_canonname_ptr <- ((\hsc_ptr -> peekByteOff hsc_ptr 32)) p
{-# LINE 124 "Network/Socket/Info.hsc" #-}

        ai_canonname <- if ai_canonname_ptr == nullPtr
                        then return Nothing
                        else Just <$> peekCString ai_canonname_ptr

        return $ AddrInfo {
            addrFlags = unpackBits aiFlagMapping ai_flags
          , addrFamily = unpackFamily ai_family
          , addrSocketType = unpackSocketType ai_socktype
          , addrProtocol = ai_protocol
          , addrAddress = ai_addr
          , addrCanonName = ai_canonname
          }

    poke p (AddrInfo flags family sockType protocol _ _) = do
        let c_stype = packSocketType sockType

        ((\hsc_ptr -> pokeByteOff hsc_ptr 0)) p (packBits aiFlagMapping flags)
{-# LINE 142 "Network/Socket/Info.hsc" #-}
        ((\hsc_ptr -> pokeByteOff hsc_ptr 4)) p (packFamily family)
{-# LINE 143 "Network/Socket/Info.hsc" #-}
        ((\hsc_ptr -> pokeByteOff hsc_ptr 8)) p c_stype
{-# LINE 144 "Network/Socket/Info.hsc" #-}
        ((\hsc_ptr -> pokeByteOff hsc_ptr 12)) p protocol
{-# LINE 145 "Network/Socket/Info.hsc" #-}

        -- stuff below is probably not needed, but let's zero it for safety

        ((\hsc_ptr -> pokeByteOff hsc_ptr 16)) p (0::CSize)
{-# LINE 149 "Network/Socket/Info.hsc" #-}
        ((\hsc_ptr -> pokeByteOff hsc_ptr 24)) p nullPtr
{-# LINE 150 "Network/Socket/Info.hsc" #-}
        ((\hsc_ptr -> pokeByteOff hsc_ptr 32)) p nullPtr
{-# LINE 151 "Network/Socket/Info.hsc" #-}
        ((\hsc_ptr -> pokeByteOff hsc_ptr 40)) p nullPtr
{-# LINE 152 "Network/Socket/Info.hsc" #-}

-- | Flags that control the querying behaviour of 'getNameInfo'.
--   For more information, see <https://tools.ietf.org/html/rfc3493#page-30>
data NameInfoFlag =
    -- | Resolve a datagram-based service name.  This is
    --   required only for the few protocols that have different port
    --   numbers for their datagram-based versions than for their
    --   stream-based versions.
      NI_DGRAM
    -- | If the hostname cannot be looked up, an IO error is thrown.
    | NI_NAMEREQD
    -- | If a host is local, return only the hostname part of the FQDN.
    | NI_NOFQDN
    -- | The name of the host is not looked up.
    --   Instead, a numeric representation of the host's
    --   address is returned.  For an IPv4 address, this will be a
    --   dotted-quad string.  For IPv6, it will be colon-separated
    --   hexadecimal.
    | NI_NUMERICHOST
    -- | The name of the service is not
    --   looked up.  Instead, a numeric representation of the
    --   service is returned.
    | NI_NUMERICSERV
    deriving (Eq, Read, Show)

niFlagMapping :: [(NameInfoFlag, CInt)]

niFlagMapping = [(NI_DGRAM, 16),
{-# LINE 180 "Network/Socket/Info.hsc" #-}
                 (NI_NAMEREQD, 8),
{-# LINE 181 "Network/Socket/Info.hsc" #-}
                 (NI_NOFQDN, 4),
{-# LINE 182 "Network/Socket/Info.hsc" #-}
                 (NI_NUMERICHOST, 1),
{-# LINE 183 "Network/Socket/Info.hsc" #-}
                 (NI_NUMERICSERV, 2)]
{-# LINE 184 "Network/Socket/Info.hsc" #-}

-- | Default hints for address lookup with 'getAddrInfo'.
--
-- >>> addrFlags defaultHints
-- []
-- >>> addrFamily defaultHints
-- AF_UNSPEC
-- >>> addrSocketType defaultHints
-- NoSocketType
-- >>> addrProtocol defaultHints
-- 0

defaultHints :: AddrInfo
defaultHints = AddrInfo {
    addrFlags      = []
  , addrFamily     = AF_UNSPEC
  , addrSocketType = NoSocketType
  , addrProtocol   = defaultProtocol
  , addrAddress    = SockAddrInet 0 0
  , addrCanonName  = Nothing
  }

class GetAddrInfo t where
    -----------------------------------------------------------------------------
    -- | Resolve a host or service name to one or more addresses.
    -- The 'AddrInfo' values that this function returns contain 'SockAddr'
    -- values that you can pass directly to 'connect' or
    -- 'bind'.
    --
    -- This function calls @getaddrinfo(3)@, which never successfully returns
    -- with an empty list. If the query fails, 'getAddrInfo' throws an IO
    -- exception.
    --
    -- For backwards-compatibility reasons, a hidden 'GetAddrInfo' class is used
    -- to make the result polymorphic. It only has instances for @[]@ (lists)
    -- and 'NonEmpty'. Use of 'NonEmpty' is recommended.
    --
    -- This function is protocol independent.  It can return both IPv4 and
    -- IPv6 address information.
    --
    -- The 'AddrInfo' argument specifies the preferred query behaviour,
    -- socket options, or protocol.  You can override these conveniently
    -- using Haskell's record update syntax on 'defaultHints', for example
    -- as follows:
    --
    -- >>> let hints = defaultHints { addrFlags = [AI_NUMERICHOST], addrSocketType = Stream }
    --
    -- You must provide a 'Just' value for at least one of the 'HostName'
    -- or 'ServiceName' arguments.  'HostName' can be either a numeric
    -- network address (dotted quad for IPv4, colon-separated hex for
    -- IPv6) or a hostname.  In the latter case, its addresses will be
    -- looked up unless 'AI_NUMERICHOST' is specified as a hint.  If you
    -- do not provide a 'HostName' value /and/ do not set 'AI_PASSIVE' as
    -- a hint, network addresses in the result will contain the address of
    -- the loopback interface.
    --
    -- There are several reasons why a query might result in several
    -- values.  For example, the queried-for host could be multihomed, or
    -- the service might be available via several protocols.
    --
    -- Note: the order of arguments is slightly different to that defined
    -- for @getaddrinfo@ in RFC 2553.  The 'AddrInfo' parameter comes first
    -- to make partial application easier.
    --
    -- >>> import qualified Data.List.NonEmpty as NE
    -- >>> addr <- NE.head <$> getAddrInfo (Just hints) (Just "127.0.0.1") (Just "http")
    -- >>> addrAddress addr
    -- 127.0.0.1:80
    --
    -- Polymorphic version: @since 3.2.3.0
    getAddrInfo
        :: Maybe AddrInfo -- ^ preferred socket type or protocol
        -> Maybe HostName -- ^ host name to look up
        -> Maybe ServiceName -- ^ service name to look up
        -> IO (t AddrInfo) -- ^ resolved addresses, with "best" first

instance GetAddrInfo [] where
    getAddrInfo = getAddrInfoList

instance GetAddrInfo NE.NonEmpty where
    getAddrInfo = getAddrInfoNE

getAddrInfoNE
    :: Maybe AddrInfo -- ^ preferred socket type or protocol
    -> Maybe HostName -- ^ host name to look up
    -> Maybe ServiceName -- ^ service name to look up
    -> IO (NonEmpty AddrInfo) -- ^ resolved addresses, with "best" first
getAddrInfoNE hints node service = alloc getaddrinfo
  where
    alloc body = withSocketsDo $ maybeWith withCString node $ \c_node ->
        maybeWith withCString service                       $ \c_service ->
            maybeWith with filteredHints                    $ \c_hints ->
                  alloca                                    $ \ptr_ptr_addrs ->
                      body c_node c_service c_hints ptr_ptr_addrs
    getaddrinfo c_node c_service c_hints ptr_ptr_addrs = mask $ \release -> do
        ret <- c_getaddrinfo c_node c_service c_hints ptr_ptr_addrs
        if ret == 0 then do
            ptr_addrs <- peek ptr_ptr_addrs
            ais       <- release (followAddrInfo ptr_addrs) `onException` c_freeaddrinfo ptr_addrs
            c_freeaddrinfo ptr_addrs
            return ais
          else do
            err <- gai_strerror ret
            ioError $ ioeSetErrorString
                        (mkIOError NoSuchThing message Nothing Nothing)
                        err
    message = concat [
        "Network.Socket.getAddrInfo (called with preferred socket type/protocol: "
      , maybe "Nothing" show hints
      , ", host name: "
      , maybe "Nothing" show node
      , ", service name: "
      , maybe "Nothing" show service
      , ")"
      ]

{-# LINE 308 "Network/Socket/Info.hsc" #-}
    filteredHints = hints

{-# LINE 310 "Network/Socket/Info.hsc" #-}

getAddrInfoList
    :: Maybe AddrInfo
    -> Maybe HostName
    -> Maybe ServiceName
    -> IO [AddrInfo]
getAddrInfoList hints node service =
    -- getAddrInfo never returns an empty list.
    NE.toList <$> getAddrInfoNE hints node service

followAddrInfo :: Ptr AddrInfo -> IO (NonEmpty AddrInfo)
followAddrInfo ptr_ai
    -- POSIX requires that getaddrinfo(3) returns at least one addrinfo.
    -- See: http://pubs.opengroup.org/onlinepubs/9699919799/functions/getaddrinfo.html
    | ptr_ai == nullPtr = ioError $ mkIOError NoSuchThing "getaddrinfo must return at least one addrinfo" Nothing Nothing
    | otherwise = do
        a <- peek ptr_ai
        ptr <- ((\hsc_ptr -> peekByteOff hsc_ptr 40)) ptr_ai
{-# LINE 328 "Network/Socket/Info.hsc" #-}
        (a :|) <$> go ptr
  where
    go :: Ptr AddrInfo -> IO [AddrInfo]
    go ptr
      | ptr == nullPtr = return []
      | otherwise = do
            a' <- peek ptr
            ptr' <- ((\hsc_ptr -> peekByteOff hsc_ptr 40)) ptr
{-# LINE 336 "Network/Socket/Info.hsc" #-}
            as' <- go ptr'
            return (a':as')

foreign import ccall safe "hsnet_getaddrinfo"
    c_getaddrinfo :: CString -> CString -> Ptr AddrInfo -> Ptr (Ptr AddrInfo)
                  -> IO CInt

foreign import ccall safe "hsnet_freeaddrinfo"
    c_freeaddrinfo :: Ptr AddrInfo -> IO ()

gai_strerror :: CInt -> IO String


{-# LINE 349 "Network/Socket/Info.hsc" #-}
gai_strerror n = c_gai_strerror n >>= peekCString

foreign import ccall safe "gai_strerror"
    c_gai_strerror :: CInt -> IO CString

{-# LINE 356 "Network/Socket/Info.hsc" #-}

-----------------------------------------------------------------------------

withCStringIf :: Bool -> Int -> (CSize -> CString -> IO a) -> IO a
withCStringIf False _ f = f 0 nullPtr
withCStringIf True  n f = allocaBytes n (f (fromIntegral n))

-- | Resolve an address to a host or service name.
-- This function is protocol independent.
-- The list of 'NameInfoFlag' values controls query behaviour.
--
-- If a host or service's name cannot be looked up, then the numeric
-- form of the address or service will be returned.
--
-- If the query fails, this function throws an IO exception.
--
-- >>> addr:_ <- getAddrInfo (Just defaultHints) (Just "127.0.0.1") (Just "http")
-- >>> getNameInfo [NI_NUMERICHOST, NI_NUMERICSERV] True True $ addrAddress addr
-- (Just "127.0.0.1",Just "80")
{-
-- >>> getNameInfo [] True True $ addrAddress addr
-- (Just "localhost",Just "http")
-}
getNameInfo
    :: [NameInfoFlag] -- ^ flags to control lookup behaviour
    -> Bool -- ^ whether to look up a hostname
    -> Bool -- ^ whether to look up a service name
    -> SockAddr -- ^ the address to look up
    -> IO (Maybe HostName, Maybe ServiceName)
getNameInfo flags doHost doService addr = alloc getnameinfo
  where
    alloc body = withSocketsDo $
        withCStringIf doHost (1025)        $ \c_hostlen c_host ->
{-# LINE 389 "Network/Socket/Info.hsc" #-}
            withCStringIf doService (32) $ \c_servlen c_serv ->
{-# LINE 390 "Network/Socket/Info.hsc" #-}
                withSockAddr addr                        $ \ptr_addr sz ->
                  body c_hostlen c_host c_servlen c_serv ptr_addr sz
    getnameinfo c_hostlen c_host c_servlen c_serv ptr_addr sz = do
        ret <- c_getnameinfo ptr_addr
                             (fromIntegral sz)
                             c_host
                             c_hostlen
                             c_serv
                             c_servlen
                             (packBits niFlagMapping flags)
        if ret == 0 then do
            let peekIf doIf c_val =
                    if doIf then Just <$> peekCString c_val else return Nothing
            host <- peekIf doHost c_host
            serv <- peekIf doService c_serv
            return (host, serv)
          else do
            err <- gai_strerror ret
            ioError $ ioeSetErrorString
                        (mkIOError NoSuchThing message Nothing Nothing)
                        err
    message = concat [
        "Network.Socket.getNameInfo (called with flags: "
      , show flags
      , ", hostname lookup: "
      , show doHost
      , ", service name lookup: "
      , show doService
      , ", socket address: "
      , show addr
      , ")"
      ]

foreign import ccall safe "hsnet_getnameinfo"
    c_getnameinfo :: Ptr SockAddr -> CInt{-CSockLen???-} -> CString -> CSize -> CString
                  -> CSize -> CInt -> IO CInt

-- | Pack a list of values into a bitmask.  The possible mappings from
-- value to bit-to-set are given as the first argument.  We assume
-- that each value can cause exactly one bit to be set; unpackBits will
-- break if this property is not true.

-----------------------------------------------------------------------------

packBits :: (Eq a, Num b, Bits b) => [(a, b)] -> [a] -> b
packBits mapping xs = foldl' pack 0 mapping
  where
    pack acc (k, v) | k `elem` xs = acc .|. v
                    | otherwise   = acc

-- | Unpack a bitmask into a list of values.
unpackBits :: (Num b, Bits b) => [(a, b)] -> b -> [a]
-- Be permissive and ignore unknown bit values. At least on OS X,
-- getaddrinfo returns an ai_flags field with bits set that have no
-- entry in <netdb.h>.
unpackBits [] _    = []
unpackBits ((k,v):xs) r
    | r .&. v /= 0 = k : unpackBits xs (r .&. complement v)
    | otherwise    = unpackBits xs r

-----------------------------------------------------------------------------
-- SockAddr

-- |
--
-- >>> SockAddrInet6 80 0 (0,0,0xffff,0x01020304) 0
-- [::ffff:1.2.3.4]:80
instance Show SockAddr where
  showsPrec _ (SockAddrUnix str) = showString str
  showsPrec _ (SockAddrInet port ha)
   = showHostAddress ha
   . showString ":"
   . shows port
  showsPrec _ (SockAddrInet6 port _ ha6 _)
   = showChar '['
   . showHostAddress6 ha6
   . showString "]:"
   . shows port


-- Taken from on the implementation of showIPv4 in Data.IP.Addr
showHostAddress :: HostAddress -> ShowS
showHostAddress ip =
  let (u3, u2, u1, u0) = hostAddressToTuple ip in
  foldr1 (.) . intersperse (showChar '.') $ map showInt [u3, u2, u1, u0]

showHostAddress' :: HostAddress -> ShowS
showHostAddress' ip =
  let (u3, u2, u1, u0) = hostAddressToTuple' ip in
  foldr1 (.) . intersperse (showChar '.') $ map showInt [u3, u2, u1, u0]

-- Taken from showIPv6 in Data.IP.Addr.

-- | Show an IPv6 address in the most appropriate notation, based on recommended
-- representation proposed by <http://tools.ietf.org/html/rfc5952 RFC 5952>.
--
-- /The implementation is completely compatible with the current implementation
-- of the `inet_ntop` function in glibc./
showHostAddress6 :: HostAddress6 -> ShowS
showHostAddress6 ha6@(a1, a2, a3, a4)
    -- IPv4-Mapped IPv6 Address
    | a1 == 0 && a2 == 0 && a3 == 0xffff =
      showString "::ffff:" . showHostAddress' a4
    -- IPv4-Compatible IPv6 Address (exclude IPRange ::/112)
    | a1 == 0 && a2 == 0 && a3 == 0 && a4 >= 0x10000 =
        showString "::" . showHostAddress' a4
    -- length of longest run > 1, replace it with "::"
    | end - begin > 1 =
        showFields prefix . showString "::" . showFields suffix
    | otherwise =
        showFields fields
  where
    fields =
        let (u7, u6, u5, u4, u3, u2, u1, u0) = hostAddress6ToTuple ha6 in
        [u7, u6, u5, u4, u3, u2, u1, u0]
    showFields = foldr (.) id . intersperse (showChar ':') . map showHex
    prefix = take begin fields  -- fields before "::"
    suffix = drop end fields    -- fields after "::"
    begin = end + diff          -- the longest run of zeros
    (diff, end) = minimum $
        scanl (\c i -> if i == 0 then c - 1 else 0) 0 fields `zip` [0..]

-----------------------------------------------------------------------------

-- | A utility function to open a socket with `AddrInfo`.
-- This is a just wrapper for the following code:
--
-- > \addr -> socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
openSocket :: AddrInfo -> IO Socket
openSocket addr = socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
