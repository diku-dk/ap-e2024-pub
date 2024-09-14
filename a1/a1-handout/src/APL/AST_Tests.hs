module APL.AST_Tests (tests) where

import APL.AST (Exp (..), printExp)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "Prettyprinting"
    [ testCase "printExp CstInt" $
        printExp (CstInt 2)
          @?= "2"
    , --
      testCase "printExp CstBool" $
        printExp (CstBool True)
          @?= "True"
    , --
      testCase "printExp Add" $
        printExp (Add (CstInt 2) (CstInt 5))
          @?= "(2 + 5)"
    , --
      testCase "printExp Sub" $
        printExp (Sub (CstInt 10) (CstInt 5))
          @?= "(10 - 5)"
    , --
      testCase "printExp Mul" $
        printExp (Mul (CstInt 6) (CstInt 2))
          @?= "(6 * 2)"
    , --
      testCase "printExp Div" $
        printExp (Div (CstInt 3) (CstInt 6))
          @?= "(3 / 6)"
    , --
      testCase "printExp Pow" $
        printExp (Pow (CstInt 2) (CstInt 5))
          @?= "(2 ** 5)"
    , --
      testCase "printExp Eql" $
        printExp (Eql (CstInt 2) (CstInt 5))
          @?= "(2 == 5)"
    , --
      testCase "printExp If" $
        printExp (If (CstBool True) (CstInt 2) (CstInt 5))
          @?= "if True then 2 else 5"
    , --
      testCase "printExp Var" $
        printExp (Var "x")
          @?= "x"
    , --
      testCase "printExp Let" $
        printExp (Let "x" (CstInt 2) (Var "x"))
          @?= "let x = 2 in x"
    , --
      testCase "printExp Lambda" $
        printExp (Lambda "x" (Add (Var "x") (CstInt 5)))
          @?= "\\x -> (x + 5)"
    , --
      testCase "printExp Apply" $
        printExp (Apply (Var "f") (CstInt 5))
          @?= "(f)(5)"
    , --
      testCase "printExp TryCatch" $
        printExp (TryCatch (CstInt 0) (CstInt 1))
          @?= "try 0 catch 1"
    ]
