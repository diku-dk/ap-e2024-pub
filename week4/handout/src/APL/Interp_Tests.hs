module APL.Interp_Tests (tests) where

import APL.AST (Exp (..))
import APL.Eval (eval)
import APL.InterpIO (runEvalIO)
import APL.InterpPure (runEval)
import APL.Monad (Error, Val (..))
import GHC.IO.Handle (hDuplicate, hDuplicateTo)
import System.IO (hClose, hFlush, hGetContents, hPutStrLn, stdin, stdout)
import System.Process (createPipe)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

eval' :: Exp -> Val
eval' = runEval . eval

evalIO' :: Exp -> IO (Either Error Val)
evalIO' = runEvalIO . eval

tests :: TestTree
tests = testGroup "Free monad interpreters" [pureTests, ioTests]

pureTests :: TestTree
pureTests =
  testGroup
    "Pure interpreter"
    []

ioTests :: TestTree
ioTests =
  testGroup
    "IO interpreter"
    []

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
