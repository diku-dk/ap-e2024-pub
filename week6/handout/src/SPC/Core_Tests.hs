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
      []
