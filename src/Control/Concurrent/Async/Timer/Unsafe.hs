{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}

module Control.Concurrent.Async.Timer.Unsafe
  ( Timer
  , defaultTimerConf
  , timerConfSetInitDelay
  , timerConfSetInterval
  , withAsyncTimer
  , timerWait
  ) where

import qualified Control.Concurrent.Async.Lifted         as Unsafe
import           Control.Concurrent.Async.Timer.Internal
import           Control.Concurrent.Lifted
import           Control.Monad
import           Control.Monad.Trans.Control

withAsyncTimer :: forall m b. (MonadBaseControl IO m)
               => TimerConf -> (Timer -> m b) -> m b
withAsyncTimer conf io = do
  mVar <- newEmptyMVar
  let timer        = Timer { timerMVar = mVar }
      timerTrigger = void $ tryPutMVar mVar ()
      initDelay'   = toMicroseconds $ _timerConfInitDelay conf
      interval'    = toMicroseconds $ _timerConfInterval  conf
      timerThread  = timerLoop (threadDelay initDelay')
                               (threadDelay interval')
                               timerTrigger
  Unsafe.withAsync timerThread $ const (io timer)

  where toMicroseconds x = x * (10 ^ 3)
