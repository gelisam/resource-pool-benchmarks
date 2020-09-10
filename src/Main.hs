
import Data.Pool
import Control.Concurrent
import Control.Concurrent.Async
import Data.IORef
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
          stripes  <- [1 .. 30],
          openTime <- [60],
          poolSize <- [1 .. 30],
          threadCount <- [1 .. 30],
          testTime <- [10]
        ]

  putStrLn "Test Time, Open Time, Stripes, Pool Size, Thread Count, Updates"

  mapM_ testRun configs

testRun :: Config -> IO ()
testRun Config {..} = do
  pool <- createPool (pure ()) (const $ pure ()) stripes openTime poolSize

  takeCounter <- newCounter 0

  threads <- replicateM threadCount $ forkIO $ forever $ withResource pool $ \_ -> incrCounter_ 1 takeCounter

  threadDelay $ testTime * 1000000
  mapM_ killThread threads

  let message count = intercalate ","
        [ show testTime
        , show openTime
        , show stripes
        , show poolSize
        , show threadCount
        , show count
        ]
  putStrLn . message =<< readCounter takeCounter
