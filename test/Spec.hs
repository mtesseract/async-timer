module Main where

import           Control.Concurrent
import           Control.Concurrent.Async.Timer
import           Control.Exception
import           Control.Monad
import           Criterion.Measurement
import           Data.Function                  ((&))
import           Data.IORef
import           Data.Typeable
import           Test.Framework                 (Test, defaultMain, testGroup)
import           Test.Framework.Providers.HUnit (testCase)
import           Test.HUnit                     ((@?=))

data MyException = MyException
    deriving (Show, Typeable)

instance Exception MyException

main :: IO ()
main = do
  cap <- getNumCapabilities
  putStrLn ""
  putStrLn $ "Cap = " ++ show cap
  defaultMain tests

tests :: [Test.Framework.Test]
tests =
  [ testGroup "1st Test Group"
    [ testCase "1st Test" test1 ]
  ]

test1 :: IO ()
test1 = do
  let conf = defaultTimerConf & timerConfSetInitDelay    0
                              & timerConfSetInterval  1000 -- ms
      noOfTicks = 5

  counter <- newIORef 0
  times <- newIORef []

  withAsyncTimer conf $ \ timer ->
    forM_ [1..noOfTicks] $ \_ -> do
      timerWait timer
      void $ forkIO $ myAction counter times

  threadDelay (5 * 10^5)
  n <- readIORef counter
  n @?= noOfTicks

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
