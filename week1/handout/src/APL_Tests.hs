module APL_Tests (tests) where

import APL ()
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ()

evalTests :: TestTree
evalTests =
  testGroup
    "Evaluation"
    []

printTests :: TestTree
printTests =
  testGroup
    "Prettyprinting"
    []

tests :: TestTree
tests = testGroup "APL" [evalTests, printTests]
