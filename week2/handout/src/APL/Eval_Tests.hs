module APL.Eval_Tests (tests) where

import APL.AST (Exp (..))
import APL.Eval (Error, Val (..), eval, runEval)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "Evaluation"
    [ testCase "CstInt" $
        runEval (eval (CstInt 42)) @?= Right (ValInt 42),
      testCase "CstBool" $
        runEval (eval (CstBool True)) @?= Right (ValBool True),
      testCase "Add" $
        runEval (eval (Add (CstInt 1) (CstInt 2))) @?= Right (ValInt 3),
      testCase "Sub" $
        runEval (eval (Sub (CstInt 1) (CstInt 2))) @?= Right (ValInt (-1)),
      testCase "Mul" $
        runEval (eval (Mul (CstInt 2) (CstInt 3))) @?= Right (ValInt 6),
      testCase "Div" $
        runEval (eval (Div (CstInt 6) (CstInt 2))) @?= Right (ValInt 3),
      testCase "Pow" $
        runEval (eval (Pow (CstInt 2) (CstInt 3))) @?= Right (ValInt 8),
      testCase "Eql" $
        runEval (eval (Eql (CstInt 2) (CstInt 3))) @?= Right (ValBool False),
      testCase "If" $
        runEval (eval (If (CstBool True) (CstInt 1) (CstInt 2))) @?= Right (ValInt 1),
      testCase "IfFalse" $
        runEval (eval (If (CstBool False) (CstInt 1) (CstInt 2))) @?= Right (ValInt 2),
      testCase "IfError" $
        runEval (eval (If (CstInt 1) (CstInt 1) (CstInt 2))) @?= Left "type error in if",
      testCase "AddError" $
        runEval (eval (Add (CstInt 1) (CstBool True))) @?= Left "type error in addition",
      testCase "SubError" $
        runEval (eval (Sub (CstInt 1) (CstBool True))) @?= Left "type error in subtraction",
      testCase "MulError" $
        runEval (eval (Mul (CstInt 1) (CstBool True))) @?= Left "type error in multiplication",
      testCase "DivError" $
        runEval (eval (Div (CstInt 1) (CstBool True))) @?= Left "type error in division",
      testCase "DivByZero" $
        runEval (eval (Div (CstInt 1) (CstInt 0))) @?= Left "division by zero",
      testCase "DivByNeg" $
        runEval (eval (Div (CstInt 1) (CstInt (-1)))) @?= Left "division by negative number",
      testCase "PowError" $
        runEval (eval (Pow (CstInt 1) (CstBool True))) @?= Left "type error in exponentiation",
      testCase "PowNeg" $
        runEval (eval (Pow (CstInt 1) (CstInt (-1)))) @?= Left "negative exponent",
      testCase "EqlError" $
        runEval (eval (Eql (CstInt 1) (CstBool True))) @?= Left "type error in equality",
      testCase "VarUnbound" $
        runEval (eval (Var "x")) @?= Left "unbound variable",
      testCase "Let" $
        runEval (eval (Let "x" (CstInt 42) (Var "x"))) @?= Right (ValInt 42),
      testCase "LetShadow" $
        runEval (eval (Let "x" (CstInt 5) (Let "x" (CstInt 2) (Var "x")))) @?= Right (ValInt 2),
      testCase "Lambda" $
        runEval (eval (Apply (Lambda "x" (Var "x")) (CstInt 42))) @?= Right (ValInt 42),
      testCase "Lambda2" $
        runEval (eval (Apply (Lambda "x" (Lambda "y" (Add (Var "x") (Var "y"))) `Apply` CstInt 1) (CstInt 2))) @?= Right (ValInt 3),
      testCase "TryCatch" $
        runEval (eval (TryCatch (Div (CstInt 1) (CstInt 0)) (CstInt 42))) @?= Right (ValInt 42)
    ]
