module SPC_Tests (tests) where

import Control.Concurrent (threadDelay)
import Data.IORef
import SPC
import Test.Tasty (TestTree, localOption, mkTimeout, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Data.Bool (Bool(True))

tests :: TestTree
tests =
  localOption (mkTimeout 5000000) $ -- Increased timeout to 5 seconds
    testGroup
      "SPC Tests from this weeks assignments"
      [ testCase "simple-job" $ do
          spc <- startSPC
          -- Add a worker
          _ <- workerAdd spc "worker1"
          ref <- newIORef False
          j <- jobAdd spc $ Job (writeIORef ref True) 1
          r1 <- jobStatus spc j
          r1 @?= JobRunning
          r2 <- jobWait spc j
          r2 @?= Just Done
          v <- readIORef ref
          v @?= True,
        testCase "timeout" $ do
          spc <- startSPC
          _ <- workerAdd spc "worker1"
          ref <- newIORef False
          j <- jobAdd spc $ Job (threadDelay 2000000 >> writeIORef ref True) 1
          r1 <- jobStatus spc j
          r1 @?= JobRunning
          r2 <- jobWait spc j
          r2 @?= Just DoneTimeout,
        testCase "cancel" $ do
          spc <- startSPC
          _ <- workerAdd spc "worker1"
          ref <- newIORef False
          j <- jobAdd spc $ Job (threadDelay 2000000 >> writeIORef ref True) 3
          r1 <- jobStatus spc j
          r1 @?= JobRunning
          jobCancel spc j
          r2 <- jobWait spc j
          r2 @?= Just DoneCancelled,
        testCase "crash" $ do
          spc <- startSPC
          _ <- workerAdd spc "worker1"
          j1 <- jobAdd spc $ Job (error "boom") 1
          r1 <- jobWait spc j1
          r1 @?= Just DoneCrashed
          -- Ensure new jobs can still work.
          ref <- newIORef False
          j2 <- jobAdd spc $ Job (writeIORef ref True) 1
          r2 <- jobWait spc j2
          r2 @?= Just Done
          v <- readIORef ref
          v @?= True
      ]
