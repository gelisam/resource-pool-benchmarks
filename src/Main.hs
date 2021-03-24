{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

import Data.Pool
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception
import Data.Atomics.Counter
import Data.IORef
import Control.Monad
import Data.Time
import Data.List


data AbstractPool pool = AbstractPool
  { abstractCreatePool   :: forall a. IO a -> (a -> IO ()) -> Int -> IO (pool a)
  , abstractWithResource :: forall a b. pool a -> (a -> IO b) -> IO b
  , abstractDestroyPool  :: forall a. pool a -> (a -> IO ()) -> IO ()
  }

dataDotPool :: AbstractPool Pool
dataDotPool = AbstractPool
  { abstractCreatePool = \create destroy poolSize -> do
      createPool
        create
        destroy
        1    -- 1 stripe
        100  -- keep the resource open for the entire test
        poolSize
  , abstractWithResource = \pool body -> do
      withResource pool body
  , abstractDestroyPool = \pool _ -> do
      destroyAllResources pool
  }


newtype IORefPool a = IORefPool
  { unIORefPool :: IORef [a] }

ioRefPool :: AbstractPool IORefPool
ioRefPool = AbstractPool
  { abstractCreatePool = \create destroy poolSize -> do
      xs <- replicateM poolSize create
      ioRef <- newIORef xs
      pure (IORefPool ioRef)
  , abstractWithResource = \(IORefPool ioRef) body -> do
      let acquire = do
            mx <- atomicModifyIORef ioRef $ \case
              [] -> ([], Nothing)
              x:xs -> (xs, Just x)
            case mx of
              Nothing -> do
                threadDelay 10  -- for some reason yield is not enough
                yield
                acquire
              Just x -> do
                pure x
          release x = do
            atomicModifyIORef ioRef $ \xs -> (x:xs, ())
      bracket acquire release body
  , abstractDestroyPool = \(IORefPool ioRef) destroy -> do
      xs <- readIORef ioRef
      mapM_ destroy xs
  }


newtype YieldingMVarPool a = YieldingMVarPool
  { unYieldingMVarPool :: MVar [a] }

yieldingMVarPool :: AbstractPool YieldingMVarPool
yieldingMVarPool = AbstractPool
  { abstractCreatePool = \create destroy poolSize -> do
      xs <- replicateM poolSize create
      mvar <- newMVar xs
      pure (YieldingMVarPool mvar)
  , abstractWithResource = \(YieldingMVarPool mvar) body -> do
      let acquire = do
            mx <- modifyMVar mvar $ \case
              [] -> do
                pure ([], Nothing)
              x:xs -> do
                pure (xs, Just x)
            case mx of
              Nothing -> do
                yield
                acquire
              Just x -> do
                pure x
          release x = do
            modifyMVar_ mvar $ \xs -> do
              pure (x:xs)
      bracket acquire release body
  , abstractDestroyPool = \(YieldingMVarPool mvar) destroy -> do
      xs <- takeMVar mvar
      mapM_ destroy xs
  }


-- the outer MVar is a lock, the inner MVar is a blocking mechanism. When the
-- pool is empty, it contains a (Left emptinessMVar) instead of a (Right []).
-- emptinessMVar is empty until the outer MVar holds a Right again, then it
-- remains full forever.
newtype BlockingMVarPool a = BlockingMVarPool
  { unBlockingMVarPool :: MVar (Either (MVar ()) [a])
  }

blockingMVarPool :: AbstractPool BlockingMVarPool
blockingMVarPool = AbstractPool
  { abstractCreatePool = \create destroy poolSize -> do
      xs <- replicateM poolSize create
      mvar <- newMVar $ Right xs
      pure (BlockingMVarPool mvar)
  , abstractWithResource = \(BlockingMVarPool mvar) body -> do
      let acquire = do
            r <- modifyMVar mvar $ \case
              Left emptinessMVar -> do
                pure (Left emptinessMVar, Left emptinessMVar)
              Right [x] -> do
                emptinessMVar <- newEmptyMVar
                pure (Left emptinessMVar, Right x)
              Right (x:xs) -> do
                pure (Right xs, Right x)
            case r of
              Left emptinessMVar -> do
                readMVar emptinessMVar
                acquire
              Right x -> do
                pure x
          release x = do
            modifyMVar_ mvar $ \case
              Left emptinessMVar -> do
                putMVar emptinessMVar ()
                pure $ Right [x]
              Right xs -> do
                pure $ Right (x:xs)
      bracket acquire release body
  , abstractDestroyPool = \(BlockingMVarPool mvar) destroy -> do
      takeMVar mvar >>= \case
        Left _ -> do
          pure ()
        Right xs -> do
          mapM_ destroy xs
  }


newtype TVarPool a = TVarPool
  { unTVarPool :: TVar [a] }

tvarPool :: AbstractPool TVarPool
tvarPool = AbstractPool
  { abstractCreatePool = \create destroy poolSize -> do
      xs <- replicateM poolSize create
      tvar <- newTVarIO xs
      pure (TVarPool tvar)
  , abstractWithResource = \(TVarPool tvar) body -> do
      let acquire = do
            atomically $ do
              readTVar tvar >>= \case
                [] -> do
                  retry
                x:xs -> do
                  writeTVar tvar xs
                  pure x
          release x = do
            atomically $ do
              xs <- readTVar tvar
              writeTVar tvar (x:xs)
      bracket acquire release body
  , abstractDestroyPool = \(TVarPool tvar) destroy -> do
      xs <- atomically $ do
        readTVar tvar
      mapM_ destroy xs
  }


data Config = Config
  { poolSize    :: Int
  , threadCount :: Int
  , testTime    :: Int
  }


-- results (higher is better):
-- Data.Pool:          479398
-- IORefPool:        30989419
-- YieldingMVarPool:   149114
-- BlockingMVarPool:   117487
-- TVarPool:           270897
main :: IO ()
main = do
  let config = Config
        { poolSize    = 6
        , threadCount = 60
        , testTime    = 6
        }
  putStrLn . ("Data.Pool: "        ++) . show =<< testAbstractPool config dataDotPool
  putStrLn . ("IORefPool: "        ++) . show =<< testAbstractPool config ioRefPool
  putStrLn . ("YieldingMVarPool: " ++) . show =<< testAbstractPool config yieldingMVarPool
  putStrLn . ("BlockingMVarPool: " ++) . show =<< testAbstractPool config blockingMVarPool
  putStrLn . ("TVarPool: "         ++) . show =<< testAbstractPool config tvarPool

testAbstractPool :: Config -> AbstractPool pool -> IO Int
testAbstractPool (Config {..}) (AbstractPool {..}) = do
  allCounters <- newIORef []
  let addCounter = do
        counter <- newCounter 0
        atomicModifyIORef allCounters $ \counters
                                     -> (counters ++ [counter], ())
        pure counter
  let removeCounter _ = do
        pure ()
  pool <- abstractCreatePool addCounter removeCounter poolSize

  threads <- replicateM threadCount $ forkIO $ forever $ abstractWithResource pool $ \counter -> do
    incrCounter_ 1 counter
    yield

  threadDelay $ testTime * 1000000
  counters <- readIORef allCounters
  counts <- mapM readCounter counters
  let total = sum counts
  mapM_ killThread threads

  abstractDestroyPool pool removeCounter
  pure total
