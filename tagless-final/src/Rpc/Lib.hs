module Rpc.Lib(postRequestRpc, getRequestRpc) where
import Network.Socket
import qualified Network.Socket.ByteString.Lazy as LBS
import Data.Binary.Put
import Data.Binary.Get
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import Data.Word (Word32)
import Data.Bits ((.|.))
import System.Random (randomIO)
import Control.Exception (Exception, throwIO)
import Data.Typeable (Typeable)
import System.IO (hPutStrLn, stderr)

data RPCException = RPCReplyError String deriving (Show, Typeable)
instance Exception RPCException

-- RPC program constants from the .x file
kvStoreProg, kvStoreVers, kvStorePostProc, kvStoreGetProc :: Word32
kvStoreProg     = 0x20000001
kvStoreVers     = 1
kvStorePostProc = 1 -- The 'POST' procedure is ID 1
kvStoreGetProc  = 2 -- The 'GET' procedure is ID 2

-- Standard Portmapper (rpcbind) constants
pmapProg, pmapVers, pmapGetPortProc :: Word32
pmapProg        = 100000
pmapVers        = 2
pmapGetPortProc = 3 -- Procedure to get the port of a registered service

-- Standard RPC constants
rpcVersion :: Word32
rpcVersion = 2

msgTypeCall :: Word32
msgTypeCall = 0

authNull :: Word32
authNull = 0

postRequestRpc :: String -> Int -> String -> IO ()
postRequestRpc host key value = withSocketsDo $ do
    serverPort <- getRemotePort host kvStoreProg kvStoreVers
    hPutStrLn stderr $ "KVStore service found on port: " ++ show serverPort
    
    hPutStrLn stderr $ "Sending POST request for key " ++ show key ++ "..."
    sendPostRequest host (show serverPort) key value
    hPutStrLn stderr "POST request sent successfully."

getRequestRpc :: String -> Int -> IO String
getRequestRpc host key = withSocketsDo $ do
    serverPort <- getRemotePort host kvStoreProg kvStoreVers
    hPutStrLn stderr $ "KVStore service found on port: " ++ show serverPort

    hPutStrLn stderr $ "Sending GET request for key " ++ show key ++ "..."
    sendGetRequest host (show serverPort) key

sendPostRequest :: HostName -> ServiceName -> Int -> String -> IO ()
sendPostRequest host port key value = do
    xid <- randomIO :: IO Word32
    let bsValue = C8.pack value
        padding = BS.replicate ((4 - (BS.length bsValue `mod` 4)) `mod` 4) 0
    
    let payload = runPut $ do
            putWord32be xid
            putWord32be msgTypeCall
            putWord32be rpcVersion
            putWord32be kvStoreProg
            putWord32be kvStoreVers
            putWord32be kvStorePostProc -- Use POST procedure ID
            putWord32be authNull >> putWord32be 0
            putWord32be authNull >> putWord32be 0
            putWord32be (fromIntegral key)
            putWord32be (fromIntegral $ BS.length bsValue)
            putByteString bsValue
            putByteString padding

    _ <- sendAndReceive host port (addRecordMarker payload)
    return ()

sendGetRequest :: HostName -> ServiceName -> Int -> IO String
sendGetRequest host port key = do
    xid <- randomIO :: IO Word32
    
    let payload = runPut $ do
            putWord32be xid
            putWord32be msgTypeCall
            putWord32be rpcVersion
            putWord32be kvStoreProg
            putWord32be kvStoreVers
            putWord32be kvStoreGetProc -- Use GET procedure ID
            putWord32be authNull >> putWord32be 0
            putWord32be authNull >> putWord32be 0
            putWord32be (fromIntegral key) -- Argument is just the key

    let request = addRecordMarker payload
    response <- sendAndReceive host port request
    
    parseGetResponse response

parseGetResponse :: B.ByteString -> IO String
parseGetResponse rawResponse =
    -- The XDR string is located after the record marker (4 bytes) and RPC header (24 bytes)
    let payload = B.drop 4 rawResponse 
        result = runGetOrFail (skip 24 >> getString) payload
    in case result of
        Left (_, _, errMsg) -> throwIO $ RPCReplyError errMsg
        Right (_, _, str)   -> return str
    where
      -- An XDR string is a 4-byte length followed by the data and padding
      getString :: Get String
      getString = do
        len <- getWord32be
        bs <- getLazyByteString (fromIntegral len)
        return $ C8.unpack (B.toStrict bs)

-- Contacts rpcbind to get the port of a service.
getRemotePort :: HostName -> Word32 -> Word32 -> IO PortNumber
getRemotePort host prog vers = do
    xid <- randomIO :: IO Word32

    let payload = runPut $ do
            putWord32be xid
            putWord32be msgTypeCall
            putWord32be rpcVersion
            putWord32be pmapProg
            putWord32be pmapVers
            putWord32be pmapGetPortProc
            putWord32be authNull >> putWord32be 0
            putWord32be authNull >> putWord32be 0
            putWord32be prog
            putWord32be vers
            putWord32be 6 -- Protocol TCP
            putWord32be 0

    let request = addRecordMarker payload
    response <- sendAndReceive host "111" request
    
    let port = runGet getWord32be (B.drop (B.length response - 4) response)
    return $ fromIntegral port

addRecordMarker :: B.ByteString -> B.ByteString
addRecordMarker payload =
    let len = fromIntegral (B.length payload) :: Word32
        marker = runPut $ putWord32be (len .|. 0x80000000)
    in marker `B.append` payload

sendAndReceive :: HostName -> ServiceName -> B.ByteString -> IO B.ByteString
sendAndReceive host port request = do
    let hints = defaultHints { addrSocketType = Stream, addrFamily = AF_INET }
    addr <- head <$> getAddrInfo (Just hints) (Just host) (Just port)
    
    sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
    connect sock (addrAddress addr)
    LBS.sendAll sock request
    response <- LBS.recv sock 4096 
    close sock
    return response
