{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Control.Concurrent.Async.Timer
  ( Timer
  , defaultTimerConf
  , timerConfSetInitDelay
  , timerConfSetInterval
  , withAsyncTimer
  , timerWait
  ) where

import           Control.Concurrent.Async.Lifted.Safe
import           Control.Concurrent.Async.Timer.Internal
import qualified Control.Concurrent.Async.Timer.Unsafe   as Unsafe
import           Control.Exception.Safe
import           Control.Monad.Trans.Control

-- | Spawn a timer thread based on the provided timer configuration
-- and then run the provided IO action, which receives the new timer
-- as an argument and call 'timerWait' on it for synchronization. When
-- the provided IO action has terminated, the timer thread will be
-- terminated also.
--
-- This functions requires the contraint @'Forall' ('Pure' m)@, which
-- means that the monad 'm' needs to satisfy @'StM' m a ~ a@ for all
-- 'a'.
withAsyncTimer :: (MonadBaseControl IO m, MonadMask m, Forall (Pure m))
               => TimerConf -> (Timer -> m b) -> m b
withAsyncTimer = Unsafe.withAsyncTimer
