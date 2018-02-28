{-|
Module      : Control.Concurrent.Async.Timer
Description : Public API for asynchronous Timers
Copyright   : (c) Moritz Clasmeier 2016, 2018
License     : BSD3
Maintainer  : mtesseract@silverratio.net
Stability   : experimental
Portability : POSIX

This module exports the public API for asynchronous timers.
-}

module Control.Concurrent.Async.Timer
  ( Timer
  , TimerConf
  , defaultConf
  , setInitDelay
  , setInterval
  , withAsyncTimer
  , wait
  , reset
  ) where

import           Control.Concurrent.Async.Timer.Internal
