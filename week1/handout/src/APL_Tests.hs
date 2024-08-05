module APL_Tests (tests) where

import APL ()
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ()

tests :: TestTree
tests =
  testGroup
    "Evaluation"
    []
