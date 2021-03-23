{-# LANGUAGE RankNTypes #-}

import Data.Pool
import Control.Concurrent
import Data.Atomics.Counter
import Control.Monad
import Data.Time
import Data.List


data Config = Config
  { poolSize    :: Int
  , threadCount :: Int
  , testTime    :: Int
  }

data AbstractPool pool = AbstractPool
  { abstractCreatePool   :: forall a. IO a -> (a -> IO ()) -> Int -> IO (pool a)
  , abstractWithResource :: forall a b. pool a -> (a -> IO b) -> IO b
  , abstractDestroyPool  :: forall a. pool a -> IO ()
  }

dataDotPool :: AbstractPool Pool
dataDotPool = AbstractPool
  { abstractCreatePool = \create destroy poolSize -> do
      createPool
        create
        destroy
        1    -- 1 stripe
        0.5  -- keep the resource open for as short as possible
        poolSize
  , abstractWithResource = \pool body -> do
      withResource pool body
  , abstractDestroyPool = \pool -> do
      destroyAllResources pool
  }

main :: IO ()
main = do
  let config = Config
        { poolSize    = 10
        , threadCount = 100
        , testTime    = 10
        }
  testAbstractPool config dataDotPool

testAbstractPool :: Config -> AbstractPool pool -> IO ()
testAbstractPool (Config {..}) (AbstractPool {..}) = do
  totalCounter <- newCounter 0
  let totalize otherCounter =
        flip incrCounter_ totalCounter =<< readCounter otherCounter
  pool <- abstractCreatePool (newCounter 0) totalize poolSize

  threads <- replicateM threadCount $ forkIO $ forever $ abstractWithResource pool $ \theCounter -> do
    incrCounter_ 1 theCounter
    yield

  threadDelay $ testTime * 1000000
  mapM_ killThread threads

  abstractDestroyPool pool

  count <- readCounter totalCounter
  print ("Data.Pool", count)


