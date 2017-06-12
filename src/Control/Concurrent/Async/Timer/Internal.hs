{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Control.Concurrent.Async.Timer.Internal
  ( Timer(..)
  , TimerConf(..)
  , TimerException(..)
  , defaultTimerConf
  , timerThread
  , timerConfSetInitDelay
  , timerConfSetInterval
  , timerWait
  ) where

import           Control.Concurrent.Lifted
import           Control.Exception.Safe
import           Control.Monad
import           Control.Monad.Base
import           Control.Monad.Trans.Control

-- | Timer specific exception; only used for a graceful termination
-- mechanism for timer threads.
data TimerException = TimerEnd deriving (Typeable, Show)

instance Exception TimerException

-- | This is the type of timer handle, which will be provided to the
-- IO action to be executed within 'withAsyncTimer'. The user can use
-- 'timerWait' on this timer to delay execution until the next timer
-- synchronization event.
newtype Timer = Timer { timerMVar :: MVar () }

-- | Type of a timer configuration.
data TimerConf = TimerConf { _timerConfInitDelay :: Int
                           , _timerConfInterval  :: Int }

-- | This exception handler acts on exceptions of type
-- 'TimerException'. What it essentially does is providing a mechanism
-- for graceful termination of timer threads by simply ignoring the
-- TimerEnd exception.
timerHandler :: Monad m => Handler m ()
timerHandler = Handler $ \case
  TimerEnd -> return ()

-- | Sleep 'dt' milliseconds.
millisleep :: MonadBase IO m => Int -> m ()
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

-- | IO action to be executed within in a timer thread.
timerThread :: (MonadBaseControl IO m, MonadCatch m) => Int -> Int -> MVar () -> m ()
timerThread initDelay intervalDelay syncMVar =
  catches (timerLoop initDelay intervalDelay syncMVar) [timerHandler]

-- | Timer loop to be executed within in a timer thread.
timerLoop :: (MonadBaseControl IO m) => Int -> Int -> MVar () -> m ()
timerLoop initDelay intervalDelay syncMVar = do
  millisleep initDelay
  forever $ putMVar syncMVar () >> millisleep intervalDelay

-- | Wait for the next synchronization event on the givem timer.
timerWait :: MonadBaseControl IO m => Timer -> m ()
timerWait = void . takeMVar . timerMVar
