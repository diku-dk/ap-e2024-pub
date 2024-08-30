module APL.Eval_Tests (tests) where

import APL.AST (Exp (..))
import APL.Eval (Val (..), eval)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "Evaluation"
    [ testCase "CstInt" $ eval [] (CstInt 42) @?= Right (ValInt 42),
      -- 2 + 3
      testCase "Add" $ eval [] (Add (CstInt 2) (CstInt 3)) @?= Right (ValInt 5),
      -- 2^3
      testCase "Pow" $ eval [] (Pow (CstInt 2) (CstInt 3)) @?= Right (ValInt 8),
      -- 2^(3+4)
      testCase "PowAdd" $ eval [] (Pow (CstInt 2) (Add (CstInt 3) (CstInt 4))) @?= Right (ValInt 128),
      -- 2+3*4
      testCase "AddMul" $ eval [] (Add (CstInt 2) (Mul (CstInt 3) (CstInt 4))) @?= Right (ValInt 14),
      -- (2+3)*4
      testCase "MulAdd" $ eval [] (Mul (Add (CstInt 2) (CstInt 3)) (CstInt 4)) @?= Right (ValInt 20),
      -- division by zero
      testCase "DivZero" $ eval [] (Div (CstInt 2) (CstInt 0)) @?= Left "Division by zero",
      -- negative exponent
      testCase "PowNeg" $ eval [] (Pow (CstInt 2) (CstInt (-1))) @?= Left "Negative exponent",
      -- 5/2
      testCase "Div" $ eval [] (Div (CstInt 5) (CstInt 2)) @?= Right (ValInt 2),
      -- CstBool True
      testCase "CstBool" $ eval [] (CstBool True) @?= Right (ValBool True),
      -- 2 == 3
      testCase "EqlFalse" $ eval [] (Eql (CstInt 2) (CstInt 3)) @?= Right (ValBool False),
      -- 2 == 2
      testCase "EqlTrue" $ eval [] (Eql (CstInt 2) (CstInt 2)) @?= Right (ValBool True),
      -- if 4 == 4 then 2 else 3
      testCase "IfTrue" $ eval [] (If (Eql (CstInt 4) (CstInt 4)) (CstInt 2) (CstInt 3)) @?= Right (ValInt 2),
      -- if 4 == 5 then 2 else 3
      testCase "IfFalse" $ eval [] (If (Eql (CstInt 4) (CstInt 5)) (CstInt 2) (CstInt 3)) @?= Right (ValInt 3),
      -- if 4 then 2 else 3
      testCase "IfTypeError" $ eval [] (If (CstInt 4) (CstInt 2) (CstInt 3)) @?= Left "Type error",
      -- Eql (CstInt 2) (CstBool True)
      testCase "EqlTypeError" $ eval [] (Eql (CstInt 2) (CstBool True)) @?= Left "Type error",
      -- if False then 2/0 else 3
      testCase "IfNoEvalDivZero" $ eval [] (If (CstBool False) (Div (CstInt 2) (CstInt 0)) (CstInt 3)) @?= Right (ValInt 3),
      -- let x = 2 in x + 3
      testCase "Let" $ eval [] (Let "x" (CstInt 2) (Add (Var "x") (CstInt 3))) @?= Right (ValInt 5),
      -- let x = 2 in x + y
      testCase "LetVarNotFound" $ eval [] (Let "x" (CstInt 2) (Add (Var "x") (Var "y"))) @?= Left "Variable not found",
      -- Var "x"
      testCase "VarLookup" $ eval [("x", ValInt 42)] (Var "x") @?= Right (ValInt 42)
    ]
