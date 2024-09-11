module APL.AST_Tests (tests) where

import APL.AST (Exp (..), printExp)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "Prettyprinting"
    [ testCase "CstInt" $
        printExp (CstInt 42) @?= "42",
      --
      testCase "CstBool True" $
        printExp (CstBool True) @?= "True",
      --
      testCase "CstBool False" $
        printExp (CstBool False) @?= "False",
      --
      testCase "Var" $
        printExp (Var "x") @?= "x",
      --
      testCase "Add" $
        printExp (Add (CstInt 2) (CstInt 5)) @?= "(2 + 5)",
      --
      testCase "Add neg" $
        printExp (Add (CstInt (-2)) (CstInt 5)) @?= "(-2 + 5)",
      testCase "Sub" $
        printExp (Sub (CstInt 2) (CstInt 5)) @?= "(2 - 5)",
      --
      testCase "Sub neg" $
        printExp (Sub (CstInt (-2)) (CstInt 5)) @?= "(-2 - 5)",
      --
      testCase "Mul" $
        printExp (Mul (CstInt 2) (CstInt 5)) @?= "(2 * 5)",
      --
      testCase "Mul neg" $
        printExp (Mul (CstInt 2) (CstInt (-5))) @?= "(2 * -5)",
      --
      testCase "Div" $
        printExp (Div (CstInt 7) (CstInt 3)) @?= "(7 / 3)",
      --
      testCase "Div neg" $
        printExp (Div (CstInt 7) (CstInt (-3))) @?= "(7 / -3)",
      --
      testCase "Pow" $
        printExp (Pow (CstInt 2) (CstInt 3)) @?= "(2 ** 3)",
      --
      testCase "Pow neg" $
        printExp (Pow (CstInt 2) (CstInt (-3))) @?= "(2 ** -3)",
      --
      testCase "Eql" $
        printExp (Eql (CstInt 2) (CstInt 5)) @?= "(2 == 5)",
      --
      testCase "If" $
        printExp (If (CstBool True) (CstInt 2) (CstInt 5)) @?= "(if True then 2 else 5)",
      --
      testCase "Let" $
        printExp (Let "x" (CstInt 2) (Add (Var "x") (CstInt 5))) @?= "(let x = 2 in (x + 5))",
      --
      testCase "Lambda" $
        printExp (Lambda "x" (Add (Var "x") (CstInt 5))) @?= "(\\ x -> (x + 5))",
      --
      testCase "App" $
        printExp (Apply (Lambda "x" (Add (Var "x") (CstInt 5))) (CstInt 2)) @?= "((\\ x -> (x + 5)) 2)",
      --
      testCase "TryCatch" $
        printExp (TryCatch (CstInt 2) (CstInt 5)) @?= "(try 2 catch 5)"
    ]
