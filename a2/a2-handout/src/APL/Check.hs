module APL.Check (checkExp, Error) where

import APL.AST (Exp (..), VName)
import Control.Monad (ap, liftM)

type Error = String

type Env = [VName]
envEmpty :: Env
envEmpty = []

newtype CheckM a = CheckM (Env -> (Env, Either Error a))

instance Functor CheckM where
  fmap = liftM

instance Applicative CheckM where
  pure x = CheckM $ \env -> (env, Right x)
  (<*>) = ap

instance Monad CheckM where
  {- x :: (Env -> (Env, Either Error a))
  - f :: (a -> CheckM b)
  -  b :: (Env -> (Env, Either Error a))
  -}
  CheckM x >>= f = CheckM $ \env ->
    case x env of
      (env', Left err) -> (env', Left err)
      (env', Right a) ->
        let CheckM b = f a
         in b env'

envExtend :: VName -> CheckM ()
envExtend v = CheckM $ \env ->
  (v : env, Right ())

checkListExp :: [Exp] -> CheckM ()
checkListExp [] = pure ()
checkListExp (x : xs) =
  do
    _ <- check x
    checkListExp xs

check :: Exp -> CheckM ()
check e =
  case e of
    (CstInt _) -> pure ()
    (CstBool _) -> pure ()
    (KvGet e1) -> checkListExp [e1]
    (Add e1 e2) -> checkListExp [e1, e2]
    (Sub e1 e2) -> checkListExp [e1, e2]
    (Mul e1 e2) -> checkListExp [e1, e2]
    (Div e1 e2) -> checkListExp [e1, e2]
    (Pow e1 e2) -> checkListExp [e1, e2]
    (Apply e1 e2) -> checkListExp [e1, e2]
    (TryCatch e1 e2) -> checkListExp [e1, e2]
    (KvPut e1 e2) -> checkListExp [e1, e2]
    (Eql e1 e2) -> checkListExp [e1, e2]
    (If e1 e2 e3) -> checkListExp [e1, e2, e3]
    (Print _ e1) -> checkListExp [e1]
    (Let v e1 e2) ->
      do
        _ <- envExtend v
        checkListExp [e1, e2]
    (Lambda v e1) ->
      do
        _ <- envExtend v
        checkListExp [e1]
    (Var v) ->
      CheckM $ \env ->
        if v `elem` env
          then (env, Right ())
          else (env, Left $ "Variable not in scope: " ++ v)

runCheck :: CheckM a -> (Env, Either Error a)
runCheck (CheckM m) = m envEmpty

checkExp :: Exp -> Maybe String
checkExp e =
  case runCheck (check e) of
    (_, Left err) -> Just err
    _ -> Nothing
