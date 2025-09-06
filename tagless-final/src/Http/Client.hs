module Http.Client(HttpM, runHttpM) where
import Http.Lib
import KVStore

-- | A newtype wrapper around IO to distinguish our HTTP implementation.
newtype HttpM a = HttpM { runHttpM :: IO a }
  deriving (Functor, Applicative, Monad)

instance KeyValueStore HttpM where
  requestPost key val = HttpM $ do
    let body = "key=" ++ show key ++ "&value=" ++ val
        request = "POST /post HTTP/1.1\r\n" ++
                  "Host: localhost\r\n" ++
                  "Content-Type: application/x-www-form-urlencoded\r\n" ++
                  "Content-Length: " ++ show (length body) ++ "\r\n" ++
                  "\r\n" ++
                  body
    _ <- executeHttp request
    return ()

  requestGet key = HttpM $ do
    let request = "GET /get?key=" ++ show key ++ " HTTP/1.1\r\n" ++
                  "Host: localhost\r\n" ++
                  "Connection: close\r\n" ++
                  "\r\n"
    executeHttp request