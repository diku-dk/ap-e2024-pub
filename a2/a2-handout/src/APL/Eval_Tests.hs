module APL.Eval_Tests (tests) where

import APL.AST (Exp (..))
import APL.Eval (Error, Val (..), eval, runEval, runEvalTest)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

eval' :: Exp -> ([String], Either Error Val)
eval' = runEval . eval

evalTest :: Exp -> (([String], [(Val, Val)]), Either Error Val)
evalTest = runEvalTest . eval

evalTests :: TestTree
evalTests =
  testGroup
    "EValuation"
    [ testCase "Add" $
        eval' (Add (CstInt 2) (CstInt 5))
          @?= ([], Right (ValInt 7)),
      --
      testCase "Add (wrong type)" $
        eval' (Add (CstInt 2) (CstBool True))
          @?= ([], Left "Non-integer operand"),
      --
      testCase "Sub" $
        eval' (Sub (CstInt 2) (CstInt 5))
          @?= ([], Right (ValInt (-3))),
      --
      testCase "Div" $
        eval' (Div (CstInt 7) (CstInt 3))
          @?= ([], Right (ValInt 2)),
      --
      testCase "Div0" $
        eval' (Div (CstInt 7) (CstInt 0))
          @?= ([], Left "Division by zero"),
      --
      testCase "Pow" $
        eval' (Pow (CstInt 2) (CstInt 3))
          @?= ([], Right (ValInt 8)),
      --
      testCase "Pow0" $
        eval' (Pow (CstInt 2) (CstInt 0))
          @?= ([], Right (ValInt 1)),
      --
      testCase "Pow negative" $
        eval' (Pow (CstInt 2) (CstInt (-1)))
          @?= ([], Left "Negative exponent"),
      --
      testCase "Eql (false)" $
        eval' (Eql (CstInt 2) (CstInt 3))
          @?= ([], Right (ValBool False)),
      --
      testCase "Eql (true)" $
        eval' (Eql (CstInt 2) (CstInt 2))
          @?= ([], Right (ValBool True)),
      --
      testCase "If" $
        eval' (If (CstBool True) (CstInt 2) (Div (CstInt 7) (CstInt 0)))
          @?= ([], Right (ValInt 2)),
      --
      testCase "Let" $
        eval' (Let "x" (Add (CstInt 2) (CstInt 3)) (Var "x"))
          @?= ([], Right (ValInt 5)),
      --
      testCase "Let (shadowing)" $
        eval'
          ( Let
              "x"
              (Add (CstInt 2) (CstInt 3))
              (Let "x" (CstBool True) (Var "x"))
          )
          @?= ([], Right (ValBool True)),
      --
      testCase "Lambda/Apply" $
        eval'
          (Apply (Lambda "x" (Mul (Var "x") (Var "x"))) (CstInt 4))
          @?= ([], Right (ValInt 16)),
      --
      testCase "TryCatch" $
        eval'
          (TryCatch (Div (CstInt 7) (CstInt 0)) (CstBool True))
          @?= ([], Right (ValBool True)),
      --
      testCase "TryCatch with print effect in e1" $
        eval'
          (TryCatch (Div (CstInt 7) (Print "Foo" (CstBool True))) (CstBool False))
          @?= (["Foo: True"], Right (ValBool False)),
      --
      testCase "TryCatch with KvPut effect in e1" $
        evalTest
          (TryCatch (Div (CstInt 7) (KvPut (CstInt 0) (CstBool True))) (CstBool False))
          @?= (([], [(ValInt 0, ValBool True)]), Right (ValBool False)),
      --
      testCase "Print Int" $
        eval'
          (Print "Hello" (CstInt 42))
          @?= (["Hello: 42"], Right (ValInt 42)),
      --
      testCase "Print (function)" $
        eval'
          (Print "Hello" (Lambda "x" (Var "x")))
          @?= (["Hello: #<fun>"], Right (ValFun [] "x" (Var "x"))),
      --
      testCase "Print Bool" $
        eval'
          (Print "Hello" (CstBool True))
          @?= (["Hello: True"], Right (ValBool True)),
      --
      testCase "Print Failure" $
        eval'
          (Let "x" (Print "Foo" (CstInt 2)) (Var "Bar"))
          @?= (["Foo: 2"], Left "Unknown variable: Bar"),
      --
      testCase
        "Print Failure Early"
        $ eval'
          (Print "Foo" (Var "Bar"))
          @?= ([], Left "Unknown variable: Bar"),
      --
      testCase "Print Multiple" $
        eval'
          (Print "Foo" (CstInt 2) `Add` Print "Bar" (CstInt 3))
          @?= (["Foo: 2", "Bar: 3"], Right (ValInt 5)),
      --
      testCase "KvPut/KvGet" $
        eval'
          (Let "x" (KvPut (CstInt 0) (CstBool True)) (KvGet (CstInt 0)))
          @?= ([], Right (ValBool True)),
      --
      testCase "KvPut/KvGet invalid key" $
        eval'
          (Let "x" (KvPut (CstInt 0) (CstBool True)) (KvGet (CstInt 1)))
          @?= ([], Left "Invalid key: ValInt 1"),
      --
      testCase "KvPut/KvGet Update" $
        eval'
          ( Let
              "x"
              (KvPut (CstInt 0) (CstBool True))
              ( Let
                  "y"
                  (KvPut (CstInt 0) (CstBool False))
                  (KvGet (CstInt 0))
              )
          )
          @?= ([], Right (ValBool False))
    ]

tests :: TestTree
tests = testGroup "APL" [evalTests]
