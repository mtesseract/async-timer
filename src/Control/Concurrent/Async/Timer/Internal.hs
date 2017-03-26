{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE MultiParamTypeClasses    #-}

module Control.Concurrent.Async.Timer.Internal where

import           ClassyPrelude

-- | Sleep 'dt' milliseconds.
millisleep :: Int64 -> IO ()
millisleep dt = threadDelay (fromIntegral dt * 10 ^ 3)

data TimerConf = TimerConf { _timerConfInitDelay :: Int
                           , _timerConfInterval  :: Int }

defaultTimerConf :: TimerConf
defaultTimerConf = TimerConf { _timerConfInitDelay =    0
                             , _timerConfInterval  = 1000 }

timerConfSetInitDelay :: Int -> TimerConf -> TimerConf
timerConfSetInitDelay n conf = conf { _timerConfInitDelay = n }

timerConfSetInterval :: Int -> TimerConf -> TimerConf
timerConfSetInterval n conf = conf { _timerConfInterval = n }

data Timer = Timer { timerMVar :: MVar () }

timerLoop :: MonadBaseControl IO m
          => m () -> m () -> m () -> m ()
timerLoop initDelay intervalDelay timerTrigger = do
  initDelay
  forever $ timerTrigger >> intervalDelay

timerWait :: MonadBaseControl IO m
          => Timer -> m ()
timerWait = void . takeMVar . timerMVar
