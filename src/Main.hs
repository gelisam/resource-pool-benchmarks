
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
  testPool $ Config
    { stripes     = 1
    , openTime    = 1
    , poolSize    = 10
    , threadCount = 100
    , testTime    = 10
    }

testPool :: Config -> IO ()
testPool Config {..} = do
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

  count <- readCounter totalCounter
  print ("Data.Pool", count)


