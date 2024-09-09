module APL.Interp_Tests (tests) where

import APL.AST (Exp (..))
import APL.Eval (eval)
import APL.InterpIO (runEvalIO)
import APL.InterpPure (runEval)
import APL.Monad
import GHC.IO.Handle (hDuplicate, hDuplicateTo)
import System.IO (hClose, hFlush, hGetContents, hPutStrLn, stdin, stdout)
import System.Process (createPipe)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

eval' :: Exp -> ([String], Either Error Val)
eval' = runEval . eval

evalIO' :: Exp -> IO (Either Error Val)
evalIO' = runEvalIO . eval

tests :: TestTree
tests = testGroup "Free monad interpreters" [pureTests, ioTests]

pureTests :: TestTree
pureTests =
  testGroup
    "Pure interpreter"
    [ testCase "localEnv" $
        runEval
          ( localEnv (const [("x", ValInt 1)]) $
              askEnv
          )
          @?= ([], Right [("x", ValInt 1)]),
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
      testCase "State" $
        runEval
          ( do
              putState [(ValInt 0, ValInt 1)]
              modifyState $ map (\(key, _) -> (key, ValInt 5))
              getState
          )
          @?= ([], Right [(ValInt 0, ValInt 5)]),
      --
      testCase "Print" $
        runEval (evalPrint "test")
          @?= (["test"], Right ()),
      --
      testCase "Error" $
        runEval
          ( do
              _ <- failure "Oh no!"
              evalPrint "test"
          )
          @?= ([], Left "Oh no!"),
      --
      testCase "Div0" $
        eval' (Div (CstInt 7) (CstInt 0))
          @?= ([], Left "Division by zero")
    ]

ioTests :: TestTree
ioTests =
  testGroup
    "IO interpreter"
    [ testCase "print" $ do
        (out, res) <-
          testIO [] $
            evalIO' $
              Print "This is also 1" $
                Print "This is 1" $
                  CstInt 1
        (out, res) @?= (["This is 1: 1", "This is also 1: 1"], Right $ ValInt 1)
    ]

-- DO NOT MODIFY
testIO :: [String] -> IO a -> IO ([String], a)
testIO inputs m = do
  stdin' <- hDuplicate stdin
  stdout' <- hDuplicate stdout

  (inR, inW) <- createPipe
  (outR, outW) <- createPipe

  inR `hDuplicateTo` stdin
  outW `hDuplicateTo` stdout

  mapM_ (hPutStrLn inW) inputs
  hFlush inW

  res <- m

  stdin' `hDuplicateTo` stdin
  stdout' `hDuplicateTo` stdout

  output <- hGetContents outR -- hGetContents closes outR
  mapM_ hClose [stdin', stdout', inR, inW, outW]

  pure (lines output, res)
