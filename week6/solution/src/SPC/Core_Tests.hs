module SPC.Core_Tests (tests) where

import Control.Concurrent (threadDelay)
import Data.IORef
import SPC.Core
import Test.Tasty (TestTree, localOption, mkTimeout, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
  localOption (mkTimeout 3000000) $
    testGroup
      "SPC (core)"
      [ testCase "simple-job" $ do
          spc <- startSPC
          ref <- newIORef False
          j <- jobAdd spc $ Job (writeIORef ref True) 1
          r1 <- jobStatus spc j
          r1 @?= JobRunning
          r2 <- jobWait spc j
          r2 @?= Done
          v <- readIORef ref
          v @?= True,
        testCase "timeout" $ do
          spc <- startSPC
          ref <- newIORef False
          j <- jobAdd spc $ Job (threadDelay 2000000 >> writeIORef ref True) 1
          r1 <- jobStatus spc j
          r1 @?= JobRunning
          r2 <- jobWait spc j
          r2 @?= DoneTimeout,
        testCase "cancel" $ do
          spc <- startSPC
          ref <- newIORef False
          j <- jobAdd spc $ Job (threadDelay 20000000 >> writeIORef ref True) 1
          r1 <- jobStatus spc j
          r1 @?= JobRunning
          jobCancel spc j
          r2 <- jobWait spc j
          r2 @?= DoneCancelled,
        testCase "crash" $ do
          spc <- startSPC
          j1 <- jobAdd spc $ Job (error "boom") 1
          r1 <- jobWait spc j1
          r1 @?= DoneCrashed
          -- Ensure new jobs can still work.
          ref <- newIORef False
          j2 <- jobAdd spc $ Job (writeIORef ref True) 1
          r2 <- jobWait spc j2
          r2 @?= Done
          v <- readIORef ref
          v @?= True
      ]
