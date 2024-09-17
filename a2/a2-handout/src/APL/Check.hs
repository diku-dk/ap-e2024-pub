module APL.Check (checkExp, Error) where

import APL.AST (Exp (..), VName)
import Control.Monad (ap, liftM)

type Error = String

type Env = [VName]

askEnv :: CheckM Env
askEnv = CheckM $ \env -> Right env

envExtend :: VName -> Env -> Env
envExtend v env = v : env

localEnv :: (Env -> Env) -> CheckM a -> CheckM a
localEnv f (CheckM m) = CheckM $ \env -> m (f env)

newtype CheckM a = CheckM (Env -> Either Error a)

instance Functor CheckM where
  fmap = liftM

instance Applicative CheckM where
  pure x = CheckM $ \_env -> Right x
  (<*>) = ap

instance Monad CheckM where
  CheckM x >>= f = CheckM $ \env ->
    case x env of
      Left err -> Left err
      Right a ->
        let CheckM y = f a
         in y env

checkTwoExp :: Exp -> Exp -> CheckM ()
checkTwoExp e1 e2 = do
  check e1
  check e2

checkThreeExp :: Exp -> Exp -> Exp -> CheckM ()
checkThreeExp e1 e2 e3 = do
  check e1
  check e2
  check e3

check :: Exp -> CheckM ()
check (Var v) = do
  env <- askEnv
  if v `elem` env
    then
      pure ()
    else CheckM $ \_ -> Left $ "Variable not in scope: " ++ v
check (Add e1 e2) = checkTwoExp e1 e2
check (Sub e1 e2) = checkTwoExp e1 e2
check (Mul e1 e2) = checkTwoExp e1 e2
check (Div e1 e2) = checkTwoExp e1 e2
check (Pow e1 e2) = checkTwoExp e1 e2
check (Eql e1 e2) = checkTwoExp e1 e2
check (If e1 e2 e3) = checkThreeExp e1 e2 e3
check (Let v e1 e2) = do
  check e1
  localEnv (envExtend v) $ check e2
check (Lambda v e) = localEnv (envExtend v) $ check e
check (Apply e1 e2) = checkTwoExp e1 e2
check (TryCatch e1 e2) = checkTwoExp e1 e2
check (Print _ e) = check e
check (KvPut _ e2) = check e2
check _ = pure ()

checkExp :: Exp -> Maybe Error
checkExp e =
  let CheckM m = check e
   in case m [] of
        Left err -> Just err
        Right _ -> Nothing
