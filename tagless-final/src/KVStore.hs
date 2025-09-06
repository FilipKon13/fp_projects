module KVStore(KeyValueStore, requestPost, requestGet) where

-- | The KeyValueStore class defines the interface for our key-value store.
-- The 'm' is a type parameter representing the monadic context in which
-- the operations are executed. This allows for different implementations.
class Monad m => KeyValueStore m where
  requestPost :: Int -> String -> m ()
  requestGet  :: Int -> m String