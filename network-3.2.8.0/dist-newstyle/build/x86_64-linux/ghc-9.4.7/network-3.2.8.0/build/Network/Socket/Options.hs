{-# LINE 1 "Network/Socket/Options.hsc" #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GADTs #-}


#include "HsNetDef.h"

module Network.Socket.Options (
    SocketOption(SockOpt
                ,UnsupportedSocketOption
                ,AcceptConn,Debug,ReuseAddr,SoDomain,Type,SoProtocol,SoError
                ,DontRoute,Broadcast,SendBuffer,RecvBuffer,KeepAlive,KeepInit
                ,OOBInline,TimeToLive,MaxSegment,NoDelay,Cork,Linger,ReusePort
                ,RecvLowWater,SendLowWater,RecvTimeOut,SendTimeOut
                ,UseLoopBack,UserTimeout,IPv6Only
                ,RecvIPv4TTL,RecvIPv4TOS,RecvIPv4PktInfo,DontFragment
                ,RecvIPv6HopLimit,RecvIPv6TClass,RecvIPv6PktInfo
                ,CustomSockOpt)
  , isSupportedSocketOption
  , whenSupported
  , getSocketType
  , getSocketOption
  , setSocketOption
  , getSockOpt
  , setSockOpt
  , SockOptValue (..)
  , setSockOptValue
  , StructLinger (..)
  , SocketTimeout (..)
  ) where

import qualified Text.Read as P

import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Utils (with)

import Network.Socket.Imports
import Network.Socket.Internal
import Network.Socket.Types
import Network.Socket.ReadShow



----------------------------------------------------------------
-- Socket Properties

-- | Socket options for use with 'setSocketOption' and 'getSocketOption'.
--
-- The existence of a constructor does not imply that the relevant option
-- is supported on your system: see 'isSupportedSocketOption'
data SocketOption = SockOpt

{-# LINE 56 "Network/Socket/Options.hsc" #-}
    CInt -- ^ Option Level
    CInt -- ^ Option Name

{-# LINE 62 "Network/Socket/Options.hsc" #-}
  deriving (Eq)

----------------------------------------------------------------

socketOptionBijection :: Bijection SocketOption String
socketOptionBijection =
    [ (UnsupportedSocketOption, "UnsupportedSocketOption")
    , (Debug, "Debug")
    , (ReuseAddr, "ReuseAddr")
    , (SoDomain, "SoDomain")
    , (Type, "Type")
    , (SoProtocol, "SoProtocol")
    , (SoError, "SoError")
    , (DontRoute, "DontRoute")
    , (Broadcast, "Broadcast")
    , (SendBuffer, "SendBuffer")
    , (RecvBuffer, "RecvBuffer")
    , (KeepAlive, "KeepAlive")
    , (KeepInit, "KeepInit")
    , (OOBInline, "OOBInline")
    , (Linger, "Linger")
    , (ReusePort, "ReusePort")
    , (RecvLowWater, "RecvLowWater")
    , (SendLowWater, "SendLowWater")
    , (RecvTimeOut, "RecvTimeOut")
    , (SendTimeOut, "SendTimeOut")
    , (UseLoopBack, "UseLoopBack")
    , (MaxSegment, "MaxSegment")
    , (NoDelay, "NoDelay")
    , (UserTimeout, "UserTimeout")
    , (Cork, "Cork")
    , (TimeToLive, "TimeToLive")
    , (RecvIPv4TTL, "RecvIPv4TTL")
    , (RecvIPv4TOS, "RecvIPv4TOS")
    , (RecvIPv4PktInfo, "RecvIPv4PktInfo")
    , (DontFragment, "DontFragment")
    , (IPv6Only, "IPv6Only")
    , (RecvIPv6HopLimit, "RecvIPv6HopLimit")
    , (RecvIPv6TClass, "RecvIPv6TClass")
    , (RecvIPv6PktInfo, "RecvIPv6PktInfo")
    ]

instance Show SocketOption where
    showsPrec = bijectiveShow socketOptionBijection def
      where
        defname = "SockOpt"
        unwrap = \(CustomSockOpt nm) -> nm
        def = defShow defname unwrap showIntInt


instance Read SocketOption where
    readPrec = bijectiveRead socketOptionBijection def
      where
        defname = "SockOpt"
        def = defRead defname CustomSockOpt readIntInt

----------------------------------------------------------------

pattern UnsupportedSocketOption :: SocketOption
pattern UnsupportedSocketOption = SockOpt (-1) (-1)

-- | Does the 'SocketOption' exist on this system?
isSupportedSocketOption :: SocketOption -> Bool
isSupportedSocketOption opt = opt /= SockOpt (-1) (-1)

-- | Execute the given action only when the specified socket option is
--  supported. Any return value is ignored.
whenSupported :: SocketOption -> IO a -> IO ()
whenSupported s action
  | isSupportedSocketOption s = action >> return ()
  | otherwise                 = return ()

----------------------------------------------------------------


{-# LINE 137 "Network/Socket/Options.hsc" #-}
-- | SO_ACCEPTCONN, read-only
pattern AcceptConn :: SocketOption

{-# LINE 140 "Network/Socket/Options.hsc" #-}
pattern AcceptConn     = SockOpt (1) (30)
{-# LINE 141 "Network/Socket/Options.hsc" #-}

{-# LINE 144 "Network/Socket/Options.hsc" #-}
-- | SO_DEBUG
pattern Debug :: SocketOption

{-# LINE 147 "Network/Socket/Options.hsc" #-}
pattern Debug          = SockOpt (1) (1)
{-# LINE 148 "Network/Socket/Options.hsc" #-}

{-# LINE 151 "Network/Socket/Options.hsc" #-}
-- | SO_REUSEADDR
pattern ReuseAddr :: SocketOption

{-# LINE 154 "Network/Socket/Options.hsc" #-}
pattern ReuseAddr      = SockOpt (1) (2)
{-# LINE 155 "Network/Socket/Options.hsc" #-}

{-# LINE 158 "Network/Socket/Options.hsc" #-}

-- | SO_DOMAIN, read-only
pattern SoDomain :: SocketOption

{-# LINE 162 "Network/Socket/Options.hsc" #-}
pattern SoDomain       = SockOpt (1) (39)
{-# LINE 163 "Network/Socket/Options.hsc" #-}

{-# LINE 166 "Network/Socket/Options.hsc" #-}

-- | SO_TYPE, read-only
pattern Type :: SocketOption

{-# LINE 170 "Network/Socket/Options.hsc" #-}
pattern Type           = SockOpt (1) (3)
{-# LINE 171 "Network/Socket/Options.hsc" #-}

{-# LINE 174 "Network/Socket/Options.hsc" #-}

-- | SO_PROTOCOL, read-only
pattern SoProtocol :: SocketOption

{-# LINE 178 "Network/Socket/Options.hsc" #-}
pattern SoProtocol     = SockOpt (1) (38)
{-# LINE 179 "Network/Socket/Options.hsc" #-}

{-# LINE 182 "Network/Socket/Options.hsc" #-}

-- | SO_ERROR
pattern SoError :: SocketOption

{-# LINE 186 "Network/Socket/Options.hsc" #-}
pattern SoError        = SockOpt (1) (4)
{-# LINE 187 "Network/Socket/Options.hsc" #-}

{-# LINE 190 "Network/Socket/Options.hsc" #-}
-- | SO_DONTROUTE
pattern DontRoute :: SocketOption

{-# LINE 193 "Network/Socket/Options.hsc" #-}
pattern DontRoute      = SockOpt (1) (5)
{-# LINE 194 "Network/Socket/Options.hsc" #-}

{-# LINE 197 "Network/Socket/Options.hsc" #-}
-- | SO_BROADCAST
pattern Broadcast :: SocketOption

{-# LINE 200 "Network/Socket/Options.hsc" #-}
pattern Broadcast      = SockOpt (1) (6)
{-# LINE 201 "Network/Socket/Options.hsc" #-}

{-# LINE 204 "Network/Socket/Options.hsc" #-}
-- | SO_SNDBUF
pattern SendBuffer :: SocketOption

{-# LINE 207 "Network/Socket/Options.hsc" #-}
pattern SendBuffer     = SockOpt (1) (7)
{-# LINE 208 "Network/Socket/Options.hsc" #-}

{-# LINE 211 "Network/Socket/Options.hsc" #-}
-- | SO_RCVBUF
pattern RecvBuffer :: SocketOption

{-# LINE 214 "Network/Socket/Options.hsc" #-}
pattern RecvBuffer     = SockOpt (1) (8)
{-# LINE 215 "Network/Socket/Options.hsc" #-}

{-# LINE 218 "Network/Socket/Options.hsc" #-}
-- | SO_KEEPALIVE
pattern KeepAlive :: SocketOption

{-# LINE 221 "Network/Socket/Options.hsc" #-}
pattern KeepAlive      = SockOpt (1) (9)
{-# LINE 222 "Network/Socket/Options.hsc" #-}

{-# LINE 225 "Network/Socket/Options.hsc" #-}
-- | TCP_KEEPINIT
pattern KeepInit :: SocketOption

{-# LINE 232 "Network/Socket/Options.hsc" #-}
pattern KeepInit       = SockOpt (-1) (-1)

{-# LINE 234 "Network/Socket/Options.hsc" #-}
-- | SO_OOBINLINE
pattern OOBInline :: SocketOption

{-# LINE 237 "Network/Socket/Options.hsc" #-}
pattern OOBInline      = SockOpt (1) (10)
{-# LINE 238 "Network/Socket/Options.hsc" #-}

{-# LINE 241 "Network/Socket/Options.hsc" #-}
-- | SO_LINGER: timeout in seconds, 0 means disabling/disabled.
pattern Linger :: SocketOption

{-# LINE 244 "Network/Socket/Options.hsc" #-}
pattern Linger         = SockOpt (1) (13)
{-# LINE 245 "Network/Socket/Options.hsc" #-}

{-# LINE 248 "Network/Socket/Options.hsc" #-}
-- | SO_REUSEPORT
pattern ReusePort :: SocketOption

{-# LINE 251 "Network/Socket/Options.hsc" #-}
pattern ReusePort      = SockOpt (1) (15)
{-# LINE 252 "Network/Socket/Options.hsc" #-}

{-# LINE 255 "Network/Socket/Options.hsc" #-}
-- | SO_RCVLOWAT
pattern RecvLowWater :: SocketOption

{-# LINE 258 "Network/Socket/Options.hsc" #-}
pattern RecvLowWater   = SockOpt (1) (18)
{-# LINE 259 "Network/Socket/Options.hsc" #-}

{-# LINE 262 "Network/Socket/Options.hsc" #-}
-- | SO_SNDLOWAT
pattern SendLowWater :: SocketOption

{-# LINE 265 "Network/Socket/Options.hsc" #-}
pattern SendLowWater   = SockOpt (1) (19)
{-# LINE 266 "Network/Socket/Options.hsc" #-}

{-# LINE 269 "Network/Socket/Options.hsc" #-}
-- | SO_RCVTIMEO: timeout in microseconds. This option is not useful
-- in the normal case where sockets are non-blocking.
pattern RecvTimeOut :: SocketOption

{-# LINE 273 "Network/Socket/Options.hsc" #-}
pattern RecvTimeOut    = SockOpt (1) (20)
{-# LINE 274 "Network/Socket/Options.hsc" #-}

{-# LINE 277 "Network/Socket/Options.hsc" #-}
-- | SO_SNDTIMEO: timeout in microseconds. This option is not useful
-- in the normal case where sockets are non-blocking.
pattern SendTimeOut :: SocketOption

{-# LINE 281 "Network/Socket/Options.hsc" #-}
pattern SendTimeOut    = SockOpt (1) (21)
{-# LINE 282 "Network/Socket/Options.hsc" #-}

{-# LINE 285 "Network/Socket/Options.hsc" #-}
-- | SO_USELOOPBACK
pattern UseLoopBack :: SocketOption

{-# LINE 290 "Network/Socket/Options.hsc" #-}
pattern UseLoopBack    = SockOpt (-1) (-1)

{-# LINE 292 "Network/Socket/Options.hsc" #-}

{-# LINE 293 "Network/Socket/Options.hsc" #-}


{-# LINE 295 "Network/Socket/Options.hsc" #-}
-- | TCP_MAXSEG
pattern MaxSegment :: SocketOption

{-# LINE 298 "Network/Socket/Options.hsc" #-}
pattern MaxSegment     = SockOpt (6) (2)
{-# LINE 299 "Network/Socket/Options.hsc" #-}

{-# LINE 302 "Network/Socket/Options.hsc" #-}
-- | TCP_NODELAY
pattern NoDelay :: SocketOption

{-# LINE 305 "Network/Socket/Options.hsc" #-}
pattern NoDelay        = SockOpt (6) (1)
{-# LINE 306 "Network/Socket/Options.hsc" #-}

{-# LINE 309 "Network/Socket/Options.hsc" #-}
-- | TCP_USER_TIMEOUT
pattern UserTimeout :: SocketOption

{-# LINE 312 "Network/Socket/Options.hsc" #-}
pattern UserTimeout    = SockOpt (6) (18)
{-# LINE 313 "Network/Socket/Options.hsc" #-}

{-# LINE 316 "Network/Socket/Options.hsc" #-}
-- | TCP_CORK
pattern Cork :: SocketOption

{-# LINE 319 "Network/Socket/Options.hsc" #-}
pattern Cork           = SockOpt (6) (3)
{-# LINE 320 "Network/Socket/Options.hsc" #-}

{-# LINE 323 "Network/Socket/Options.hsc" #-}

{-# LINE 324 "Network/Socket/Options.hsc" #-}


{-# LINE 326 "Network/Socket/Options.hsc" #-}
-- | IP_TTL
pattern TimeToLive :: SocketOption

{-# LINE 329 "Network/Socket/Options.hsc" #-}
pattern TimeToLive     = SockOpt (0) (2)
{-# LINE 330 "Network/Socket/Options.hsc" #-}

{-# LINE 333 "Network/Socket/Options.hsc" #-}
-- | Receiving IPv4 TTL.
pattern RecvIPv4TTL :: SocketOption

{-# LINE 336 "Network/Socket/Options.hsc" #-}
pattern RecvIPv4TTL    = SockOpt (0) (12)
{-# LINE 337 "Network/Socket/Options.hsc" #-}

{-# LINE 340 "Network/Socket/Options.hsc" #-}
-- | Receiving IPv4 TOS.
pattern RecvIPv4TOS :: SocketOption

{-# LINE 343 "Network/Socket/Options.hsc" #-}
pattern RecvIPv4TOS    = SockOpt (0) (13)
{-# LINE 344 "Network/Socket/Options.hsc" #-}

{-# LINE 347 "Network/Socket/Options.hsc" #-}
-- | Receiving IP_PKTINFO (struct in_pktinfo).
pattern RecvIPv4PktInfo :: SocketOption

{-# LINE 352 "Network/Socket/Options.hsc" #-}
pattern RecvIPv4PktInfo  = SockOpt (0) (8)
{-# LINE 353 "Network/Socket/Options.hsc" #-}

{-# LINE 356 "Network/Socket/Options.hsc" #-}
-- | IP_DONTFRAG
pattern DontFragment :: SocketOption

{-# LINE 361 "Network/Socket/Options.hsc" #-}
pattern DontFragment   = SockOpt (0) (10)
{-# LINE 362 "Network/Socket/Options.hsc" #-}

{-# LINE 365 "Network/Socket/Options.hsc" #-}

{-# LINE 366 "Network/Socket/Options.hsc" #-}


{-# LINE 368 "Network/Socket/Options.hsc" #-}
-- | IPV6_V6ONLY: don't use this on OpenBSD.
pattern IPv6Only :: SocketOption

{-# LINE 371 "Network/Socket/Options.hsc" #-}
pattern IPv6Only       = SockOpt (41) (26)
{-# LINE 372 "Network/Socket/Options.hsc" #-}

{-# LINE 375 "Network/Socket/Options.hsc" #-}
-- | Receiving IPv6 hop limit.
pattern RecvIPv6HopLimit :: SocketOption

{-# LINE 378 "Network/Socket/Options.hsc" #-}
pattern RecvIPv6HopLimit = SockOpt (41) (51)
{-# LINE 379 "Network/Socket/Options.hsc" #-}

{-# LINE 382 "Network/Socket/Options.hsc" #-}
-- | Receiving IPv6 traffic class.
pattern RecvIPv6TClass :: SocketOption

{-# LINE 385 "Network/Socket/Options.hsc" #-}
pattern RecvIPv6TClass  = SockOpt (41) (66)
{-# LINE 386 "Network/Socket/Options.hsc" #-}

{-# LINE 389 "Network/Socket/Options.hsc" #-}
-- | Receiving IPV6_PKTINFO (struct in6_pktinfo).
pattern RecvIPv6PktInfo :: SocketOption

{-# LINE 392 "Network/Socket/Options.hsc" #-}
pattern RecvIPv6PktInfo = SockOpt (41) (49)
{-# LINE 393 "Network/Socket/Options.hsc" #-}

{-# LINE 398 "Network/Socket/Options.hsc" #-}

{-# LINE 399 "Network/Socket/Options.hsc" #-}

pattern CustomSockOpt :: (CInt, CInt) -> SocketOption
pattern CustomSockOpt xy <- ((\(SockOpt x y) -> (x, y)) -> xy)
  where
    CustomSockOpt (x, y) = SockOpt x y

----------------------------------------------------------------

-- | Set a socket option that expects an 'Int' value.
setSocketOption :: Socket
                -> SocketOption -- Option Name
                -> Int          -- Option Value
                -> IO ()

{-# LINE 413 "Network/Socket/Options.hsc" #-}
setSocketOption s so@Linger v = do
    let arg = if v == 0 then StructLinger 0 0 else StructLinger 1 (fromIntegral v)
    setSockOpt s so arg

{-# LINE 417 "Network/Socket/Options.hsc" #-}
setSocketOption s so@RecvTimeOut v = setSockOpt s so $ SocketTimeout $ fromIntegral v
setSocketOption s so@SendTimeOut v = setSockOpt s so $ SocketTimeout $ fromIntegral v
setSocketOption s sa v = setSockOpt s sa (fromIntegral v :: CInt)

-- | Set a socket option.
setSockOpt :: Storable a
           => Socket
           -> SocketOption
           -> a
           -> IO ()
setSockOpt s (SockOpt level opt) v = do
    with v $ \ptr -> void $ do
        let sz = fromIntegral $ sizeOf v
        withFdSocket s $ \fd ->
          throwSocketErrorIfMinus1_ "Network.Socket.setSockOpt" $
          c_setsockopt fd level opt ptr sz

-- | Set a socket option value
--
-- The existential 'SockOptValue' enables things like:
--
-- @
-- mapM_ (uncurry $ 'setSockOptValue' sock) [
--       ('NoDelay', 'SockOptValue' @Int 1)
--     , ('Linger', 'SockOptValue' ('StructLinger' 1 0))
--     ]
-- @
setSockOptValue :: Socket
                     -> SocketOption
                     -> SockOptValue
                     -> IO ()
setSockOptValue s opt (SockOptValue v) = setSockOpt s opt v

----------------------------------------------------------------

-- | Get a socket option that gives an 'Int' value.
getSocketOption :: Socket
                -> SocketOption  -- Option Name
                -> IO Int        -- Option Value

{-# LINE 457 "Network/Socket/Options.hsc" #-}
getSocketOption s so@Linger = do
    StructLinger onoff linger <- getSockOpt s so
    return $ fromIntegral $ if onoff == 0 then 0 else linger

{-# LINE 461 "Network/Socket/Options.hsc" #-}
getSocketOption s so@RecvTimeOut = do
    SocketTimeout to <- getSockOpt s so
    return $ fromIntegral to
getSocketOption s so@SendTimeOut = do
    SocketTimeout to <- getSockOpt s so
    return $ fromIntegral to
getSocketOption s so = do
    n :: CInt <- getSockOpt s so
    return $ fromIntegral n

-- | Get a socket option.
getSockOpt :: forall a . Storable a
           => Socket
           -> SocketOption -- Option Name
           -> IO a         -- Option Value
getSockOpt s (SockOpt level opt) = do
    alloca $ \ptr -> do
        let sz = fromIntegral $ sizeOf (undefined :: a)
        withFdSocket s $ \fd -> with sz $ \ptr_sz -> do
            throwSocketErrorIfMinus1Retry_ "Network.Socket.getSockOpt" $
                c_getsockopt fd level opt ptr ptr_sz
        peek ptr

----------------------------------------------------------------

-- | Get the 'SocketType' of an active socket.
--
--   Since: 3.0.1.0
getSocketType :: Socket -> IO SocketType
getSocketType s = unpackSocketType <$> getSockOpt s Type

----------------------------------------------------------------


{-# LINE 495 "Network/Socket/Options.hsc" #-}
{-# COMPLETE CustomSockOpt #-}

{-# LINE 497 "Network/Socket/Options.hsc" #-}

{-# LINE 498 "Network/Socket/Options.hsc" #-}
-- | Low level @SO_LINGER@ option value, which can be used with 'setSockOpt' or
-- @'setSockOptValue' . 'SockOptValue'@.
data StructLinger = StructLinger {
    -- | Set the linger option on.
    sl_onoff  :: CInt,

    -- | Linger timeout.
    sl_linger :: CInt
  }
  deriving (Eq, Ord, Show)

instance Storable StructLinger where
    sizeOf    ~_ = (8)
{-# LINE 511 "Network/Socket/Options.hsc" #-}
    alignment ~_ = alignment (0 :: CInt)

    peek p = do
        onoff  <- ((\hsc_ptr -> peekByteOff hsc_ptr 0)) p
{-# LINE 515 "Network/Socket/Options.hsc" #-}
        linger <- ((\hsc_ptr -> peekByteOff hsc_ptr 4)) p
{-# LINE 516 "Network/Socket/Options.hsc" #-}
        return $ StructLinger onoff linger

    poke p (StructLinger onoff linger) = do
        ((\hsc_ptr -> pokeByteOff hsc_ptr 0))  p onoff
{-# LINE 520 "Network/Socket/Options.hsc" #-}
        ((\hsc_ptr -> pokeByteOff hsc_ptr 4)) p linger
{-# LINE 521 "Network/Socket/Options.hsc" #-}

{-# LINE 522 "Network/Socket/Options.hsc" #-}

-- | A type that can hold any 'Storable' socket option value (e.g.
-- 'StructLinger' and 'CInt')
--
-- See 'setSocOptValue'
data SockOptValue where
  SockOptValue :: Storable a => a -> SockOptValue

----------------------------------------------------------------

-- | Timeout in microseconds.
--   This will be converted into struct timeval on Unix and
--   DWORD (as milliseconds) on Windows.
newtype SocketTimeout = SocketTimeout Word32 deriving (Eq, Ord, Show)


{-# LINE 546 "Network/Socket/Options.hsc" #-}
instance Storable SocketTimeout where
    sizeOf    ~_ = ((16))
{-# LINE 548 "Network/Socket/Options.hsc" #-}
    alignment ~_ = (8)
{-# LINE 549 "Network/Socket/Options.hsc" #-}
    peek ptr    = do
            sec  <- ((\hsc_ptr -> peekByteOff hsc_ptr 0))  ptr
{-# LINE 551 "Network/Socket/Options.hsc" #-}
            usec <- ((\hsc_ptr -> peekByteOff hsc_ptr 8)) ptr
{-# LINE 552 "Network/Socket/Options.hsc" #-}
            return $ SocketTimeout (sec * 1000000 + usec)
    poke ptr (SocketTimeout to) = do
            let (sec, usec) = to `divMod` 1000000
            ((\hsc_ptr -> pokeByteOff hsc_ptr 0))  ptr sec
{-# LINE 556 "Network/Socket/Options.hsc" #-}
            ((\hsc_ptr -> pokeByteOff hsc_ptr 8)) ptr usec
{-# LINE 557 "Network/Socket/Options.hsc" #-}

{-# LINE 558 "Network/Socket/Options.hsc" #-}

----------------------------------------------------------------

foreign import CALLCONV unsafe "getsockopt"
  c_getsockopt :: CInt -> CInt -> CInt -> Ptr a -> Ptr CInt -> IO CInt
foreign import CALLCONV unsafe "setsockopt"
  c_setsockopt :: CInt -> CInt -> CInt -> Ptr a -> CInt -> IO CInt
