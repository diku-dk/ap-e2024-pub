module APL_Tests (tests) where

import APL (Error, Exp (..), Val (..), eval, runEval)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

eval' :: Exp -> Either Error Val
eval' = runEval . eval

evalTests :: TestTree
evalTests =
  testGroup
    "Evaluation"
    []

tests :: TestTree
tests = testGroup "APL" [evalTests]
