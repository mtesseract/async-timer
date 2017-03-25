# async-timer

Example:

      let conf = defaultTimerConf & timerConfSetInitDelay  500 -- 500 ms
                                  & timerConfSetInterval  1000 -- 1 s
    
      withAsyncTimer conf $ \ timer -> do
        forM_ [1..10] $ \_ -> do
          timerWait timer
          putStrLn "Tick"
