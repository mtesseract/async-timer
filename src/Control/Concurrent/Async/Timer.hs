module Control.Concurrent.Async.Timer
  ( Timer
  , defaultTimerConf
  , timerConfSetInitDelay
  , timerConfSetInterval
  , withAsyncTimer
  , timerWait
  ) where

import           Control.Concurrent.Async.Timer.Internal

-- | Spawn a timer thread based on the provided timer configuration
-- and then run the provided IO action, which receives the new timer
-- as an argument and call 'timerWait' on it for synchronization. When
-- the provided IO action has terminated, the timer thread will be
-- terminated also.
--
-- This functions requires the contraint @'Forall' ('Pure' m)@, which
-- means that the monad 'm' needs to satisfy @'StM' m a ~ a@ for all
-- 'a'.
