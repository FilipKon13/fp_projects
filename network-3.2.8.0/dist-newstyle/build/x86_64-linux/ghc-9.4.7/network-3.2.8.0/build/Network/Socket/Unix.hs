{-# LINE 1 "Network/Socket/Unix.hsc" #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}


#include "HsNetDef.h"

module Network.Socket.Unix (
    isUnixDomainSocketAvailable
  , socketPair
  , sendFd
  , recvFd
  , getPeerCredential
  , getPeerCred
  , getPeerEid
  ) where

import Foreign.Marshal.Alloc (allocaBytes)
import Network.Socket.Buffer
import Network.Socket.Fcntl
import Network.Socket.Imports
import Network.Socket.Types
import System.Posix.Types (Fd(..))


{-# LINE 31 "Network/Socket/Unix.hsc" #-}
import Foreign.Marshal.Array (peekArray)
import Network.Socket.Internal
import Network.Socket.Posix.Cmsg

{-# LINE 35 "Network/Socket/Unix.hsc" #-}


{-# LINE 39 "Network/Socket/Unix.hsc" #-}

{-# LINE 42 "Network/Socket/Unix.hsc" #-}


{-# LINE 44 "Network/Socket/Unix.hsc" #-}
import Network.Socket.Options

{-# LINE 46 "Network/Socket/Unix.hsc" #-}

-- | Getting process ID, user ID and group ID for UNIX-domain sockets.
--
--   This is implemented with SO_PEERCRED on Linux and getpeereid()
--   on BSD variants. Unfortunately, on some BSD variants
--   getpeereid() returns unexpected results, rather than an error,
--   for AF_INET sockets. It is the user's responsibility to make sure
--   that the socket is a UNIX-domain socket.
--   Also, on some BSD variants, getpeereid() does not return credentials
--   for sockets created via 'socketPair', only separately created and then
--   explicitly connected UNIX-domain sockets work on such systems.
--
--   Since 2.7.0.0.
getPeerCredential :: Socket -> IO (Maybe CUInt, Maybe CUInt, Maybe CUInt)

{-# LINE 61 "Network/Socket/Unix.hsc" #-}
getPeerCredential sock = do
    (pid, uid, gid) <- getPeerCred sock
    if uid == maxBound then
        return (Nothing, Nothing, Nothing)
      else
        return (Just pid, Just uid, Just gid)

{-# LINE 77 "Network/Socket/Unix.hsc" #-}

-- | Returns the processID, userID and groupID of the peer of
--   a UNIX-domain socket.
--
-- Only available on platforms that support SO_PEERCRED.
getPeerCred :: Socket -> IO (CUInt, CUInt, CUInt)

{-# LINE 84 "Network/Socket/Unix.hsc" #-}
getPeerCred s = do
    let opt = SockOpt (1) (17)
{-# LINE 86 "Network/Socket/Unix.hsc" #-}
    PeerCred cred <- getSockOpt s opt
    return cred

newtype PeerCred = PeerCred (CUInt, CUInt, CUInt)
instance Storable PeerCred where
    sizeOf    ~_ = (12)
{-# LINE 92 "Network/Socket/Unix.hsc" #-}
    alignment ~_ = alignment (0 :: CInt)
    poke _ _ = return ()
    peek p = do
        pid <- ((\hsc_ptr -> peekByteOff hsc_ptr 0)) p
{-# LINE 96 "Network/Socket/Unix.hsc" #-}
        uid <- ((\hsc_ptr -> peekByteOff hsc_ptr 4)) p
{-# LINE 97 "Network/Socket/Unix.hsc" #-}
        gid <- ((\hsc_ptr -> peekByteOff hsc_ptr 8)) p
{-# LINE 98 "Network/Socket/Unix.hsc" #-}
        return $ PeerCred (pid, uid, gid)

{-# LINE 102 "Network/Socket/Unix.hsc" #-}
{-# Deprecated getPeerCred "Use getPeerCredential instead" #-}

-- | Returns the userID and groupID of the peer of
--   a UNIX-domain socket.
--
--  Only available on platforms that support getpeereid().
getPeerEid :: Socket -> IO (CUInt, CUInt)

{-# LINE 123 "Network/Socket/Unix.hsc" #-}
getPeerEid _ = return (0, 0)

{-# LINE 125 "Network/Socket/Unix.hsc" #-}

{-# Deprecated getPeerEid "Use getPeerCredential instead" #-}

-- | Whether or not UNIX-domain sockets are available.
--   'AF_UNIX' is supported on Windows since 3.1.3.0.
--   So, this variable is 'True` on all platforms.
--
--   Since 2.7.0.0.
isUnixDomainSocketAvailable :: Bool
isUnixDomainSocketAvailable = True

-- | Send a file descriptor over a UNIX-domain socket.
--   This function does not work on Windows.
sendFd :: Socket -> CInt -> IO ()
sendFd s outfd = void $ allocaBytes dummyBufSize $ \buf -> do
    let cmsg = encodeCmsg [Fd outfd]
    sendBufMsg s NullSockAddr [(buf,dummyBufSize)] [cmsg] mempty
  where
    dummyBufSize = 1

-- | Receive a file descriptor over a UNIX-domain socket. Note that the resulting
--   file descriptor may have to be put into non-blocking mode in order to be
--   used safely. See 'setNonBlockIfNeeded'.
--   This function does not work on Windows.
recvFd :: Socket -> IO CInt
recvFd s = allocaBytes dummyBufSize $ \buf -> do
    (NullSockAddr, _, cmsgs, _) <- recvBufMsg s [(buf,dummyBufSize)] 32 mempty
    case (lookupCmsg CmsgIdFds cmsgs >>= decodeCmsg) :: Maybe [Fd] of
      Just (Fd fd : _) -> return fd
      _                -> return (-1)
  where
    dummyBufSize = 16

-- | Build a pair of connected socket objects.
--   On Windows, this function emulates socketpair() using
--   'AF_UNIX' and a temporary file will remain.
socketPair :: Family              -- Family Name (usually AF_UNIX)
           -> SocketType          -- Socket Type (usually Stream)
           -> ProtocolNumber      -- Protocol Number
           -> IO (Socket, Socket) -- unnamed and connected.

{-# LINE 180 "Network/Socket/Unix.hsc" #-}
socketPair family stype protocol =
    allocaBytes (2 * sizeOf (1 :: CInt)) $ \ fdArr -> do
      let c_stype = packSocketType stype
      _rc <- throwSocketErrorIfMinus1Retry "Network.Socket.socketpair" $
                  c_socketpair (packFamily family) c_stype protocol fdArr
      [fd1,fd2] <- peekArray 2 fdArr
      setNonBlockIfNeeded fd1
      setNonBlockIfNeeded fd2
      s1 <- mkSocket fd1
      s2 <- mkSocket fd2
      return (s1, s2)

foreign import ccall unsafe "socketpair"
  c_socketpair :: CInt -> CInt -> CInt -> Ptr CInt -> IO CInt

{-# LINE 195 "Network/Socket/Unix.hsc" #-}
