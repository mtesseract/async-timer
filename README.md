# async-timer [![Hackage version](https://img.shields.io/hackage/v/async-timer.svg?label=Hackage)](https://hackage.haskell.org/package/async-timer) [![Stackage version](https://www.stackage.org/package/async-timer/badge/lts?label=Stackage)](https://www.stackage.org/package/async-timer) [![Build Status](https://travis-ci.org/mtesseract/async-timer.svg?branch=master)](https://travis-ci.org/mtesseract/async-timer)

### About

This is a lightweight package built on top of the async package
providing easy to use periodic timers. This can be used for executing
IO actions periodically.

### Example:

```haskell
      let conf = defaultTimerConf & timerConfSetInitDelay  500 -- 500 ms
                                  & timerConfSetInterval  1000 -- 1 s
    
      withAsyncTimer conf $ \ timer -> do
        forM_ [1..10] $ \_ -> do
          timerWait timer
          putStrLn "Tick"
```