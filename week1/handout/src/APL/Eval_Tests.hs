module APL.Eval_Tests (tests) where

import APL.AST ()
import APL.Eval ()
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ()

tests :: TestTree
tests =
  testGroup
    "Evaluation"
    []
