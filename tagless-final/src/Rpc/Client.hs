module Rpc.Client(RpcM, runRpcM) where
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad
import Rpc.Lib
import KVStore

newtype RpcM a = RpcM { runRpcM :: IO a }
  deriving (Functor, Applicative, Monad)

rpcHost :: String
rpcHost = "127.0.0.1"

instance KeyValueStore RpcM where
  requestPost key val = RpcM $ do
    _ <- postRequestRpc rpcHost key val
    return ()
  requestGet key = RpcM $ getRequestRpc rpcHost key

