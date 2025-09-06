import qualified Data.Map as Map
import System.Environment (getArgs, getProgName)
import Control.Monad (when)
import Http.Client
import Test.Client
import Rpc.Client
import KVStore
import Data.Map (Map)
import System.IO (hPutStrLn, stderr)

-- A polymorphic function that uses the KeyValueStore interface.
-- It can be run with any backend (RpcM, HttpM, or TestM).
exampleInteraction :: KeyValueStore m => m ()
exampleInteraction = do
  let v1 = "First value"
  let v2 = "Second value"
  let v3 = "Third value"
  requestPost 1 v1
  requestPost 2 v2
  requestPost 3 v3
  r2 <- requestGet 2
  r3 <- requestGet 3
  r1 <- requestGet 1
  when (r1 /= v1) $ error "Value mismatch for key 1"
  when (r2 /= v2) $ error "Value mismatch for key 2"
  when (r3 /= v3) $ error "Value mismatch for key 3"
  return ()

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["rpc", "post", keyStr, val] -> do
      hPutStrLn stderr $ "Executing RPC POST: key=" ++ keyStr ++ ", val=" ++ val
      runRpcM $ requestPost (read keyStr) val
      hPutStrLn stderr "OK"

    ["rpc", "get", keyStr] -> do
      hPutStrLn stderr $ "Executing RPC GET: key=" ++ keyStr
      result <- runRpcM $ requestGet (read keyStr)
      hPutStrLn stderr $ "Result: " ++ result
      putStrLn result

    ["http", "post", keyStr, val] -> do
      hPutStrLn stderr $ "Executing HTTP POST: key=" ++ keyStr ++ ", val=" ++ val
      runHttpM $ requestPost (read keyStr) val
      hPutStrLn stderr "OK"

    ["http", "get", keyStr] -> do
      hPutStrLn stderr $ "Executing HTTP GET: key=" ++ keyStr
      result <- runHttpM $ requestGet (read keyStr)
      hPutStrLn stderr $ "Result: " ++ result
      putStrLn result

    ["run", backend] -> do
      putStrLn $ "Executing example interaction for backend: " ++ backend
      case backend of
        "rpc"  -> runRpcM exampleInteraction
        "http" -> runHttpM exampleInteraction
        "test" -> runTestM exampleInteraction
        _      -> hPutStrLn stderr "Unknown backend. Use 'rpc', 'http', or 'test'."

    _ -> do
      prog <- getProgName
      hPutStrLn stderr $ "Usage:\n" ++
                  prog ++ " rpc|http post <key> <value>\n" ++
                  prog ++ " rpc|http get <key>\n" ++
                  prog ++ " run rpc|http|test"
