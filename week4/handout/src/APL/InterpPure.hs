module APL.InterpPure (runEval) where

import APL.Monad

runEval :: EvalM a -> a
runEval = runEval' envEmpty
  where
    runEval' :: Env -> EvalM a -> a
    runEval' _ (Pure x) = error "TODO"
    runEval' r (Free (ReadOp k)) = error "TODO"
