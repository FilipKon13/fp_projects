{-# LINE 1 "Network/Socket/Posix/MsgHdr.hsc" #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

-- | Support module for the POSIX 'sendmsg' system call.
module Network.Socket.Posix.MsgHdr
    ( MsgHdr(..)
    ) where




import Network.Socket.Imports
import Network.Socket.Internal (zeroMemory)

import Network.Socket.Posix.IOVec (IOVec)

data MsgHdr sa = MsgHdr
    { msgName    :: Ptr sa
    , msgNameLen :: Word32
{-# LINE 19 "Network/Socket/Posix/MsgHdr.hsc" #-}
    , msgIov     :: Ptr IOVec

{-# LINE 21 "Network/Socket/Posix/MsgHdr.hsc" #-}
    , msgIovLen  :: CSize

{-# LINE 25 "Network/Socket/Posix/MsgHdr.hsc" #-}
    , msgCtrl    :: Ptr Word8

{-# LINE 27 "Network/Socket/Posix/MsgHdr.hsc" #-}
    , msgCtrlLen :: CSize

{-# LINE 31 "Network/Socket/Posix/MsgHdr.hsc" #-}
    , msgFlags   :: CInt
    }

instance Storable (MsgHdr sa) where
  sizeOf    ~_ = (56)
{-# LINE 36 "Network/Socket/Posix/MsgHdr.hsc" #-}
  alignment ~_ = alignment (0 :: CInt)

  peek p = do
    name       <- ((\hsc_ptr -> peekByteOff hsc_ptr 0))       p
{-# LINE 40 "Network/Socket/Posix/MsgHdr.hsc" #-}
    nameLen    <- ((\hsc_ptr -> peekByteOff hsc_ptr 8))    p
{-# LINE 41 "Network/Socket/Posix/MsgHdr.hsc" #-}
    iov        <- ((\hsc_ptr -> peekByteOff hsc_ptr 16))        p
{-# LINE 42 "Network/Socket/Posix/MsgHdr.hsc" #-}
    iovLen     <- ((\hsc_ptr -> peekByteOff hsc_ptr 24))     p
{-# LINE 43 "Network/Socket/Posix/MsgHdr.hsc" #-}
    ctrl       <- ((\hsc_ptr -> peekByteOff hsc_ptr 32))    p
{-# LINE 44 "Network/Socket/Posix/MsgHdr.hsc" #-}
    ctrlLen    <- ((\hsc_ptr -> peekByteOff hsc_ptr 40)) p
{-# LINE 45 "Network/Socket/Posix/MsgHdr.hsc" #-}
    flags      <- ((\hsc_ptr -> peekByteOff hsc_ptr 48))      p
{-# LINE 46 "Network/Socket/Posix/MsgHdr.hsc" #-}
    return $ MsgHdr name nameLen iov iovLen ctrl ctrlLen flags

  poke p mh = do
    -- We need to zero the msg_control, msg_controllen, and msg_flags
    -- fields, but they only exist on some platforms (e.g. not on
    -- Solaris).  Instead of using CPP, we zero the entire struct.
    zeroMemory p (56)
{-# LINE 53 "Network/Socket/Posix/MsgHdr.hsc" #-}
    ((\hsc_ptr -> pokeByteOff hsc_ptr 0))       p (msgName       mh)
{-# LINE 54 "Network/Socket/Posix/MsgHdr.hsc" #-}
    ((\hsc_ptr -> pokeByteOff hsc_ptr 8))    p (msgNameLen    mh)
{-# LINE 55 "Network/Socket/Posix/MsgHdr.hsc" #-}
    ((\hsc_ptr -> pokeByteOff hsc_ptr 16))        p (msgIov        mh)
{-# LINE 56 "Network/Socket/Posix/MsgHdr.hsc" #-}
    ((\hsc_ptr -> pokeByteOff hsc_ptr 24))     p (msgIovLen     mh)
{-# LINE 57 "Network/Socket/Posix/MsgHdr.hsc" #-}
    ((\hsc_ptr -> pokeByteOff hsc_ptr 32))    p (msgCtrl       mh)
{-# LINE 58 "Network/Socket/Posix/MsgHdr.hsc" #-}
    ((\hsc_ptr -> pokeByteOff hsc_ptr 40)) p (msgCtrlLen    mh)
{-# LINE 59 "Network/Socket/Posix/MsgHdr.hsc" #-}
    ((\hsc_ptr -> pokeByteOff hsc_ptr 48))      p (msgFlags      mh)
{-# LINE 60 "Network/Socket/Posix/MsgHdr.hsc" #-}
