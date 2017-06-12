{-# LANGUAGE FlexibleContexts      #-}

module Control.Concurrent.Async.Timer.Unsafe
  ( Timer
  , defaultTimerConf
  , timerConfSetInitDelay
  , timerConfSetInterval
  , withAsyncTimer
  , timerWait
  ) where

import           Control.Concurrent.Async.Lifted
import           Control.Concurrent.Async.Timer.Internal
import           Control.Concurrent.Lifted
import           Control.Exception.Safe
import           Control.Monad.Trans.Control

-- | Spawn a timer thread based on the provided timer configuration
-- and then run the provided IO action, which receives the new timer
-- as an argument and call 'timerWait' on it for synchronization. When
-- the provided IO action has terminated, the timer thread will be
-- terminated also.
withAsyncTimer :: (MonadBaseControl IO m, MonadMask m)
               => TimerConf -> (Timer -> m b) -> m b
withAsyncTimer conf io = do
  -- This MVar will be our synchronization mechanism.
  mVar <- newEmptyMVar
  let timer         = Timer { timerMVar = mVar }
      initDelay     = _timerConfInitDelay conf
      intervalDelay = _timerConfInterval  conf
  withAsync (timerThread initDelay intervalDelay mVar) $ \asyncHandle -> do
    -- This guarantees that we will be informed right away if our
    -- timer thread disappears, for example because of an async
    -- exception:
    link asyncHandle
    -- This guarantees that we will throw the TimerEnd exception to
    -- the timer thread after the provided IO action has ended
    -- (w/ or w/o an exception):
    io timer `finally` cancelWith asyncHandle TimerEnd
