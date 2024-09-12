module APL.Interp_Tests (tests) where

import APL.AST (Exp (..))
import APL.Eval (eval)
import APL.InterpIO (runEvalIO)
import APL.InterpPure (runEval)
import APL.Monad (Error, Val (..))
import Control.Concurrent (threadDelay)
import Control.Exception (bracket)
import GHC.IO.Handle (hDuplicate, hDuplicateTo)
import System.IO
import System.Process (createPipe)
import Test.Tasty (TestTree, localOption, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.Runners (NumThreads (..))

eval' :: Exp -> Val
eval' = runEval . eval

evalIO' :: Exp -> IO (Either Error Val)
evalIO' = runEvalIO . eval

tests :: TestTree
tests = testGroup "Free monad interpreters" [pureTests, ioTests']
  where
    ioTests' = localOption (NumThreads 1) ioTests

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
  hFlush stdout
  threadDelay 10000 -- Needed to make sure things are actually flushed
  stdin' <- hDuplicate stdin
  stdout' <- hDuplicate stdout

  (inR, inW) <- createPipe
  (outR, outW) <- createPipe

  hSetBuffering inW NoBuffering
  hSetBuffering outW NoBuffering

  bracket
    ( do
        inR `hDuplicateTo` stdin
        outW `hDuplicateTo` stdout
    )
    ( \_ -> do
        stdin' `hDuplicateTo` stdin
        stdout' `hDuplicateTo` stdout
        mapM_ hClose [stdin', stdout', inR, inW, outW]
    )
    ( \_ -> do
        mapM_ (hPutStrLn inW) inputs
        hFlush inW

        res <- m

        output <- hGetContents outR -- hGetContents closes outR
        pure (lines output, res)
    )
