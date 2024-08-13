module APL.InterpIO (runEvalIO) where

import APL.Monad

runEvalIO :: EvalM a -> IO a
runEvalIO = runEvalIO' envEmpty
  where
    runEvalIO' :: Env -> EvalM a -> IO a
    runEvalIO' _ (Pure x) = pure x
    runEvalIO' r (Free (ReadOp k)) = runEvalIO' r $ k r

-- Uncomment once you've defined 'PrintOp'.
-- runEvalIO' r (Free (PrintOp w m)) = do
--  appendFile "log.txt" $ concat w ++ "\n"
--  runEvalIO' r m
