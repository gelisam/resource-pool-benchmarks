
import Data.Pool
import Control.Concurrent
import Data.Atomics.Counter
import Control.Monad
import Data.Time
import Data.List

data Config = Config
  { stripes     :: Int
  , openTime    :: NominalDiffTime
  , poolSize    :: Int
  , threadCount :: Int
  , testTime    :: Int
  }

main :: IO ()
main = do
  let configs = [ Config {..} |
          stripes  <- ([1 .. 9] ++ [10, 20 .. 99] ++ [100, 200 .. 999] ++ [1000, 2000]),
          openTime <- [60],
          poolSize <- ([1 .. 9] ++ [10, 20 .. 99] ++ [100, 200 .. 999] ++ [1000, 2000]),
          threadCount <- ([10, 20 .. 99] ++ [100, 200 .. 999] ++ [1000, 2000]),
          testTime <- [10]
        ]

  putStrLn "Test Time, Open Time, Stripes, Pool Size, Thread Count, Updates"

  mapM_ testRun $ filter (\Config {..} -> stripes * poolSize == (threadCount * 2)) $ reverse configs

testRun :: Config -> IO ()
testRun Config {..} = do
  totalCounter <- newCounter 0
  let totalize otherCounter =
        flip incrCounter_ totalCounter =<< readCounter otherCounter
  pool <- createPool (newCounter 0) totalize stripes openTime poolSize

  threads <- replicateM threadCount $ forkIO $ forever $ withResource pool $ \theCounter -> do
    incrCounter_ 1 theCounter
    yield

  threadDelay $ testTime * 1000000
  mapM_ killThread threads

  destroyAllResources pool

  let message count = intercalate ","
        [ show testTime
        , show openTime
        , show stripes
        , show poolSize
        , show threadCount
        , show count
        ]
  putStrLn . message =<< readCounter totalCounter
