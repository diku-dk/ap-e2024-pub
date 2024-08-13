module APL.InterpPure (runEval) where

import APL.Monad

runEval :: EvalM a -> a
runEval = runEval' envEmpty
  where
    runEval' :: Env -> EvalM a -> a
    runEval' _ (Pure x) = undefined
    runEval' r (Free (ReadOp k)) = undefined
