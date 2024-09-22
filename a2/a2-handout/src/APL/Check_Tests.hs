module APL.Check_Tests (tests) where

import APL.AST (Exp (..))
import APL.Check (checkExp)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

-- Assert that the provided expression should pass the type checker.
testPos :: Exp -> TestTree
testPos e =
  testCase (show e) $
    checkExp e @?= Nothing

-- Assert that the provided expression should fail the type checker.
testNeg :: Exp -> TestTree
testNeg e =
  testCase (show e) $
    case checkExp e of
      Nothing -> assertFailure "expected error"
      Just _ -> pure ()

tests :: TestTree
tests =
  testGroup
    "Checking"
    [ testPos (Add (CstInt 2) (CstInt 5)),
      testNeg (Add (CstInt 2) (Var "x")),
      testPos (Sub (CstInt 2) (CstInt 5)),
      testNeg (Sub (CstInt 2) (Var "x")),
      testPos (Mul (CstInt 2) (CstInt 5)),
      testNeg (Mul (CstInt 2) (Var "x")),
      testPos (Div (CstInt 2) (CstInt 5)),
      testNeg (Div (CstInt 2) (Var "x")),
      testPos (Pow (CstInt 2) (CstInt 5)),
      testNeg (Pow (CstInt 2) (Var "x")),
      testPos (Eql (CstInt 2) (CstInt 5)),
      testNeg (Eql (CstInt 2) (Var "x")),
      testPos (If (CstBool True) (CstInt 2) (CstInt 5)),
      testNeg (If (Var "x") (CstInt 2) (CstInt 5)),
      testPos (Let "x" (CstInt 2) (Var "x")),
      testNeg (Let "x" (CstInt 2) (Var "y")),
      testPos (Lambda "x" (Var "x")),
      testNeg (Lambda "x" (Var "y")),
      testPos (Apply (Lambda "x" (Var "x")) (CstInt 2)),
      testNeg (Apply (Lambda "x" (Var "x")) (Var "y")),
      testPos (TryCatch (CstInt 2) (CstInt 5)),
      testNeg (TryCatch (Var "x") (CstInt 5)),
      testPos (Print "x" (CstInt 2)),
      testNeg (Print "x" (Var "y")),
      -- KvPut and KvGet can never fail the type checker.
      -- Some more advanced tests that involve multiple expressions.
      testPos (Let "x" (CstInt 2) (Add (Var "x") (CstInt 5))),
      testNeg (Let "x" (CstInt 2) (Add (Var "y") (CstInt 5))),
      testPos (Let "x" (CstInt 2) (Let "y" (CstInt 5) (Add (Var "x") (Var "y")))),
      testNeg (Let "x" (CstInt 2) (Let "y" (CstInt 5) (Add (Var "x") (Var "z"))))
    ]
