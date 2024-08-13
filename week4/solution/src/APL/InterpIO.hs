module APL.InterpIO (runEvalIO) where

import APL.Monad

runEvalIO :: EvalM a -> IO (Either Error a)
runEvalIO = runEvalIO' envEmpty stateInitial
  where
    runEvalIO' :: Env -> State -> EvalM a -> IO (Either Error a)
    runEvalIO' _ _ (Pure x) = pure $ pure x
    runEvalIO' r s (Free (ReadOp k)) = runEvalIO' r s $ k r
    runEvalIO' r s (Free (StateGetOp k)) = runEvalIO' r s $ k s
    runEvalIO' r _ (Free (StatePutOp s' m)) = runEvalIO' r s' m
    runEvalIO' r s (Free (PrintOp p m)) = do
      putStrLn p
      runEvalIO' r s m
    runEvalIO' _ _ (Free (ErrorOp e)) = pure $ Left e
