module APL.Eval_Tests (tests) where

import APL.AST (Exp (..))
import APL.Eval (Error, Val (..), eval, runEval)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

eval' :: Exp -> ([String], Either Error Val)
eval' = runEval . eval

evalTests :: TestTree
evalTests =
  testGroup
    "EValuation"
    [ testCase "Add" $
        eval' (Add (CstInt 2) (CstInt 5))
          @?= ([], Right (ValInt 7))
    , --
      testCase "Add (wrong type)" $
        eval' (Add (CstInt 2) (CstBool True))
          @?= ([], Left "Non-integer operand")
    , --
      testCase "Sub" $
        eval' (Sub (CstInt 2) (CstInt 5))
          @?= ([], Right (ValInt (-3)))
    , --
      testCase "Div" $
        eval' (Div (CstInt 7) (CstInt 3))
          @?= ([], Right (ValInt 2))
    , --
      testCase "Div0" $
        eval' (Div (CstInt 7) (CstInt 0))
          @?= ([], Left "Division by zero")
    , --
      testCase "Pow" $
        eval' (Pow (CstInt 2) (CstInt 3))
          @?= ([], Right (ValInt 8))
    , --
      testCase "Pow0" $
        eval' (Pow (CstInt 2) (CstInt 0))
          @?= ([], Right (ValInt 1))
    , --
      testCase "Pow negative" $
        eval' (Pow (CstInt 2) (CstInt (-1)))
          @?= ([], Left "Negative exponent")
    , --
      testCase "Eql (false)" $
        eval' (Eql (CstInt 2) (CstInt 3))
          @?= ([], Right (ValBool False))
    , --
      testCase "Eql (true)" $
        eval' (Eql (CstInt 2) (CstInt 2))
          @?= ([], Right (ValBool True))
    , --
      testCase "If" $
        eval' (If (CstBool True) (CstInt 2) (Div (CstInt 7) (CstInt 0)))
          @?= ([], Right (ValInt 2))
    , --
      testCase "Let" $
        eval' (Let "x" (Add (CstInt 2) (CstInt 3)) (Var "x"))
          @?= ([], Right (ValInt 5))
    , --
      testCase "Let (shadowing)" $
        eval'
          ( Let
              "x"
              (Add (CstInt 2) (CstInt 3))
              (Let "x" (CstBool True) (Var "x"))
          )
          @?= ([], Right (ValBool True))
    , --
      testCase "Lambda/Apply" $
        eval'
          (Apply (Lambda "x" (Mul (Var "x") (Var "x"))) (CstInt 4))
          @?= ([], Right (ValInt 16))
    , --
      testCase "TryCatch" $
        eval'
          (TryCatch (Div (CstInt 7) (CstInt 0)) (CstBool True))
          @?= ([], Right (ValBool True))
    , -- Print
      testCase "Print int" $ 
        eval' 
          (Print "x" (CstInt 2))
          @?= (["x: 2"], Right (ValInt 2))
    , --
      testCase "Print bool" $ 
        eval' 
          (Print "x" (CstBool True))
          @?= (["x: True"], Right (ValBool True))
    , --
      testCase "Print fun" $
        eval' 
          (Print "x" (Lambda "x" (Var "x")))
          @?= (["x: #<fun>"], Right (ValFun [] "x" (Var "x")))
    , -- KvPut
      testCase "KvPut" $
        eval'
          (KvPut (CstInt 3) (CstInt 4))
          @?= ([], Right (ValInt 4))
    , --
      testCase "KvPut replace existing key" $
        eval'
          (Let "x" (KvPut (CstInt 3) (CstInt 4)) (KvPut (CstInt 3) (CstInt 5)))
          @?= ([], Right (ValInt 5))
    , --
      testCase "KvPut with expression" $
        eval'
          (KvPut (Add (CstInt 1 ) (CstInt 2)) (CstInt 4))
          @?= ([], Right (ValInt 4))
    , --
      testCase "KvPut with expression value" $
        eval'
          (KvPut (CstInt 3) (Add (CstInt 2) (CstInt 2)))
          @?= ([], Right (ValInt 4))
    , -- 
      testCase "KvPut (Shadowing)" $
        eval'
          (Let "x" (KvPut (CstInt 3) (CstInt 4)) (Let "x" (CstInt 5) (Var "x")))
          @?= ([], Right (ValInt 5))
    , -- KvGet
      testCase "KvGet non-existing key" $
        eval'
          (KvGet (CstInt 5))
          @?= ([], Left "Invalid key: ValInt 5")
    , --
      testCase "KvGet" $
        eval'
          (Let "x" (KvPut (CstInt 3) (CstInt 4)) (KvGet (CstInt 3)))
          @?= ([], Right (ValInt 4))
    , --
      testCase "KvGet with expression key" $
        eval'
          (Let "x" (KvPut (CstInt 3) (CstInt 4)) (KvGet (Add (CstInt 1) (CstInt 2))))
          @?= ([], Right (ValInt 4))
    ]

tests :: TestTree
tests = testGroup "APL" [evalTests]
