{-# LINE 1 "Network/Socket/Cbits.hsc" #-}
module Network.Socket.Cbits where



import Network.Socket.Imports

-- | This is the value of SOMAXCONN, typically 128.
-- 128 is good enough for normal network servers but
-- is too small for high performance servers.
maxListenQueue :: Int
maxListenQueue = 4096
{-# LINE 12 "Network/Socket/Cbits.hsc" #-}


{-# LINE 17 "Network/Socket/Cbits.hsc" #-}
fGetFd :: CInt
fGetFd = 1
{-# LINE 19 "Network/Socket/Cbits.hsc" #-}
fGetFl :: CInt
fGetFl = 3
{-# LINE 21 "Network/Socket/Cbits.hsc" #-}
fdCloexec :: CInt
fdCloexec = 1
{-# LINE 23 "Network/Socket/Cbits.hsc" #-}
oNonBlock :: CInt
oNonBlock = 2048
{-# LINE 25 "Network/Socket/Cbits.hsc" #-}

{-# LINE 26 "Network/Socket/Cbits.hsc" #-}
sockNonBlock :: CInt
sockNonBlock = 2048
{-# LINE 28 "Network/Socket/Cbits.hsc" #-}
sockCloexec :: CInt
sockCloexec = 524288
{-# LINE 30 "Network/Socket/Cbits.hsc" #-}

{-# LINE 31 "Network/Socket/Cbits.hsc" #-}

{-# LINE 32 "Network/Socket/Cbits.hsc" #-}
