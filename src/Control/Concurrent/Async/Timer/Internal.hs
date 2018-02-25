{-# LANGUAGE LambdaCase #-}

module Control.Concurrent.Async.Timer.Internal where

import qualified Control.Concurrent.Async as Async
import           Control.Exception.Safe
import           Control.Monad            (void)
import           Control.Monad.IO.Unlift
import           UnliftIO.Async
import           UnliftIO.Concurrent
import           UnliftIO.STM

-- | This is the type of timer handle, which will be provided to the
-- IO action to be executed within 'withAsyncTimer'. The user can use
-- 'timerWait' on this timer to delay execution until the next timer
-- synchronization event.
data Timer = Timer { timerMVar    :: MVar ()
                   , timerControl :: TBQueue TimerCommand }

data TimerCommand = TimerReset deriving (Show, Eq)

-- | Type of a timer configuration.
data TimerConf = TimerConf { _timerConfInitDelay :: Int
                           , _timerConfInterval  :: Int }

-- | Sleep 'dt' milliseconds.
millisleep :: MonadIO m => Int -> m ()
millisleep dt = threadDelay (fromIntegral dt * 10 ^ 3)

-- | Default timer configuration specifies no initial delay and an
-- interval delay of 1s.
defaultTimerConf :: TimerConf
defaultTimerConf = TimerConf { _timerConfInitDelay =    0
                             , _timerConfInterval  = 1000 }

-- | Set the initial delay in the provided timer configuration.
timerConfSetInitDelay :: Int -> TimerConf -> TimerConf
timerConfSetInitDelay n conf = conf { _timerConfInitDelay = n }

-- | Set the interval delay in the provided timer configuration.
timerConfSetInterval :: Int -> TimerConf -> TimerConf
timerConfSetInterval n conf = conf { _timerConfInterval = n }

-- | Timer loop to be executed within in a timer thread.
timerLoop :: MonadUnliftIO m => Int -> Int -> Timer -> m ()
timerLoop initDelay intervalDelay timer = go initDelay

  where go delay = do
          race (millisleep delay) readCmd >>= \ case
            Left () -> do
              wakeUp
              go intervalDelay
            Right cmd ->
              case cmd of
                TimerReset ->
                  go intervalDelay

        wakeUp   = putMVar (timerMVar timer) ()
        readCmd  = atomically $ readTBQueue (timerControl timer)

-- | Wait for the next synchronization event on the givem timer.
timerWait :: MonadUnliftIO m => Timer -> m ()
timerWait = void . takeMVar . timerMVar

-- | Reset the provided timer.
timerReset :: MonadUnliftIO m => Timer -> m ()
timerReset timer =
  atomically $ writeTBQueue (timerControl timer) TimerReset

-- | Spawn a timer thread based on the provided timer configuration
-- and then run the provided IO action, which receives the new timer
-- as an argument and call 'timerWait' on it for synchronization. When
-- the provided IO action has terminated, the timer thread will be
-- terminated also.
withAsyncTimer
  :: (MonadUnliftIO m, MonadMask m)
  => TimerConf
  -> (Timer -> m b)
  -> m b
withAsyncTimer conf io = do
  -- This MVar will be our synchronization mechanism.
  mVar <- newEmptyMVar
  controlChannel <- atomically $ newTBQueue 1
  let timer         = Timer { timerMVar = mVar
                            , timerControl = controlChannel }
      initDelay     = _timerConfInitDelay conf
      intervalDelay = _timerConfInterval  conf
  withAsync (timerLoop initDelay intervalDelay timer) $ \ asyncTimer -> do
    -- This guarantees that we will be informed right away if our
    -- timer thread disappears, for example because of an async
    -- exception:
    liftIO $ Async.link asyncTimer
    io timer
