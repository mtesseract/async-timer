module Main where

import           Control.Concurrent
import           Control.Concurrent.Async
import qualified Control.Concurrent.Async.Timer as Timer
import           Control.Exception
import           Control.Monad
import           Criterion.Measurement
import           Data.Function                  ((&))
import           Data.IORef
import           Data.Typeable
import           Test.Tasty
import           Test.Tasty.HUnit

data MyException = MyException
    deriving (Show, Typeable)

instance Exception MyException

main :: IO ()
main = defaultMain (testGroup "Unit Tests" unitTests)

unitTests :: [TestTree]
unitTests =
  [ testGroup "Timer Tests"
    [ testCase "Timer reset" testTimerReset
    , testCase "Simple timer ticks" testSimpleTimerTicks
    ]
  ]

timedAssert :: Int -> IO () -> IO () -> IO ()
timedAssert delay before after = do
  threadDelay $ delay - epsilon
  before
  threadDelay $ 2 * epsilon
  after

  where epsilon = 2 * 10^5

testTimerReset :: IO ()
testTimerReset = do
  let conf = Timer.defaultConf
             & Timer.setInitDelay 1000
             & Timer.setInterval  1000 -- ms
      noOfTicks = 3

  counter <- newIORef 0

  _handle <- async $
    Timer.withAsyncTimer conf $ \ timer ->
    forM_ [1..noOfTicks] $ \ idx -> do
    when (idx == 2) $ do
      threadDelay (5 * 10^5)
      Timer.reset timer
    Timer.wait timer
    modifyIORef counter (+ 1)

  timedAssert (2 * 10^6 + 5 * 10^5)
              ((1 @=?) =<< readIORef counter)
              ((2 @=?) =<< readIORef counter)

testSimpleTimerTicks :: IO ()
testSimpleTimerTicks = do
  let conf = Timer.defaultConf
             & Timer.setInitDelay    0
             & Timer.setInterval  1000 -- ms
      noOfTicks = 3

  counter <- newIORef 0
  times <- newIORef []

  Timer.withAsyncTimer conf $ \ timer ->
    forM_ [1..noOfTicks] $ \_ -> do
      Timer.wait timer
      void $ forkIO $ myAction counter times

  threadDelay (5 * 10^5)
  readIORef counter >>= (noOfTicks @=?)

  ts <- readIORef times
  let deltas = case ts of
                 []         -> []
                 _ : tsTail -> zipWith (-) ts tsTail

      avgDiff = sum (map (subtract 1) deltas) / fromIntegral (length deltas)
  forM_ deltas (\ dt -> putStrLn $ "dt = " ++ show dt)
  putStrLn $ "average dt = " ++ show avgDiff
  return ()

  where myAction :: IORef Int -> IORef [Double] -> IO ()
        myAction counter times = do
          t <- getTime
          n <- readIORef counter
          let n' = n + 1
          writeIORef counter n'
          modifyIORef times (t :)
          putStrLn $ "Tick no. " ++ show n' ++ " (t = " ++ show t ++ ")"
          threadDelay 500000
