module Test.Client(TestM, runTestM) where
import Data.IORef (IORef, newIORef, readIORef, modifyIORef')
import Data.Map (Map, findWithDefault, insert)
import KVStore

newtype TestM a = TestM { run :: IORef (Map Int String) -> IO a }

instance Functor TestM where
  fmap f (TestM m) = TestM $ \ref -> fmap f (m ref)

instance Applicative TestM where
  pure x = TestM $ \_ -> pure x
  (TestM f) <*> (TestM x) = TestM $ \ref -> f ref <*> x ref

instance Monad TestM where
  (TestM m) >>= f = TestM $ \ref -> do
    result <- m ref
    run (f result) ref

instance KeyValueStore TestM where
  requestPost key val = TestM $ \ref ->
    modifyIORef' ref (insert key val)
  requestGet key = TestM $ \ref -> do
    m <- readIORef ref
    return $ findWithDefault "" key m

runTestM :: TestM a -> IO a
runTestM (TestM m) = do
  ref <- newIORef mempty -- Start with an empty map.
  m ref
