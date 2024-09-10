module APL.Eval_Tests (tests) where

import APL.AST (Exp(..))
import APL.Eval (Val(..), eval, envEmpty)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "Evaluation"
    [testCase "Evaluate CstInt" $
     eval envEmpty (CstInt 108) @?= Right (ValInt 108),
     --
     testCase "Add 2 and 3" $
     eval envEmpty (Add (CstInt 2) (CstInt 3)) @?= Right (ValInt 5),
     --
     testCase "Pow 2 and 3" $
     eval envEmpty (Pow (CstInt 2) (CstInt 3)) @?= Right (ValInt 8),
     --
     testCase "Pow 2 to the (2+3) exponent" $
     eval envEmpty (Pow (CstInt 2) (Add (CstInt 2) (CstInt 3))) @?= Right (ValInt 32),
     --
     testCase "Add 2 and 3 Mul 4" $
     eval envEmpty (Add (CstInt 3) (Mul (CstInt 3) (CstInt 4))) @?= Right (ValInt 15),
     --
     testCase "Mul (2+3) and 4" $
     eval envEmpty (Mul (Add (CstInt 2) (CstInt 3)) (CstInt 4)) @?= Right (ValInt 20),
     --
     testCase "eval let exp 1" $ 
     eval envEmpty (Let "x" (CstInt 3) (Add (Var "x") (Var "x"))) @?= Right (ValInt 6),
     --
     testCase "eval let exp 2" $
     eval envEmpty (Let "x" (CstInt 3) (Add (Var "x") (Var "y"))) @?= Left "variable y was not found"
     ]
