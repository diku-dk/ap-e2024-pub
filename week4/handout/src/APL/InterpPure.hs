module APL.InterpPure (runEval) where

import APL.Monad

runEval :: EvalM a -> ([String], a)
runEval = runEval' envEmpty stateInitial
  where
    runEval' :: Env -> State -> EvalM a -> ([String], a)
    runEval' _ _ (Pure x) = ([], x)
    runEval' r s (Free (ReadOp k)) = runEval' r s $ k r
    runEval' r s (Free (StateGetOp k)) = runEval' r s $ k s
    runEval' r _ (Free (StatePutOp state m)) = runEval' r state m
    runEval' r s (Free (PrintOp str m)) = (str : ss, x)
      where
        (ss, x) = runEval' r s m
