module Http.Lib(executeHttp) where
import Network.Socket
import System.IO

-----------------------------------------------------------------------------
-- 2. HTTP Backend Implementation (HttpM)
-----------------------------------------------------------------------------

-- HTTP Configuration
httpHost :: HostName
httpHost = "127.0.0.1"

httpPort :: PortNumber
httpPort = 2435

executeHttp :: String -> IO String
executeHttp request = withSocketsDo $ do
    addr <- head <$> getAddrInfo (Just defaultHints) (Just httpHost) (Just (show httpPort))
    sock <- socket (addrFamily addr) Stream defaultProtocol
    connect sock (addrAddress addr)
    handle <- socketToHandle sock ReadWriteMode
    hPutStr handle request
    hFlush handle
    -- Read headers until an empty line, then the body.
    let readHeaders = do
          line <- hGetLine handle
          if line == "\r" || line == ""
            then return ()
            else readHeaders
    readHeaders
    hGetContents handle

