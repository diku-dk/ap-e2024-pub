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
    [ {- The argument for not testing all the different available options is, that
       - they make use of the checkListExp function all of them. If that function
       - works, then they are fundamentally except does that do not use it.-}
      testPos $
        Lambda "x" (Var "x")
    , testPos $
        CstInt 2
    , testPos $
        CstBool True
    , testNeg $
        Var "x"
    , testNeg $
        Lambda "y" (Var "x")
    , testPos $
        Lambda "x" (Lambda "y" (Add (Var "x") (Var "y")))
    , testNeg $
        Lambda "z" (Lambda "y" (Add (Var "x") (Var "y")))
    , testPos $
        Let "x" (CstInt 3) (Var "x")
    , testNeg $
        Add (Var "x") (CstInt 2)
    , testPos $
        Let "x" (CstInt 1) (Add (Var "x") (CstInt 2))
    , testNeg $
        Let "x" (CstInt 1) (Var "y")
    , testPos $
        Lambda "x" (Let "y" (Var "x") (Var "y"))
    , testNeg $
        Lambda "x" (Let "y" (Var "z") (Var "y"))
    ]
