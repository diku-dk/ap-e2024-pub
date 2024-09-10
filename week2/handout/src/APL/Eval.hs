module APL.Eval
  ( Val (..),
    eval,
    runEval,
    Error,
  )
where

import APL.AST (Exp (..), VName)
import Control.Monad (ap, liftM)

data Val
  = ValInt Integer
  | ValBool Bool
  | ValFun Env VName Exp
  deriving (Eq, Show)

type Env = [(VName, Val)]

envEmpty :: Env
envEmpty = []

envExtend :: VName -> Val -> Env -> Env
envExtend v val env = (v, val) : env

envLookup :: VName -> Env -> Maybe Val
envLookup v env = lookup v env

type Error = String

newtype EvalM a = EvalM (Env -> Either Error a)

instance Functor EvalM where
  -- fmap :: (a -> b) -> EvalM a -> EvalM b
  fmap = liftM

instance Applicative EvalM where
  -- pure :: a -> EvalM a
  pure a = EvalM (\_ -> Right a)

  -- (<*>) :: EvalM (a -> b) -> EvalM a -> EvalM b
  (<*>) = ap

instance Monad EvalM where
  -- (>>=) :: EvalM a -> (a -> EvalM b) -> EvalM b
  EvalM x >>= f =
    EvalM
      ( \env -> case x env of
          Left err -> Left err
          Right a -> let EvalM y = f a in y env
      )

runEval :: EvalM a -> Either Error a
runEval (EvalM x) = x envEmpty

failure :: Error -> EvalM a
failure err = EvalM (\_ -> Left err)

askEnv :: EvalM Env
askEnv = EvalM (\env -> Right env)

localEnv :: (Env -> Env) -> EvalM a -> EvalM a
localEnv f (EvalM x) = EvalM (x . f)

catch :: EvalM a -> EvalM a -> EvalM a
catch (EvalM x) (EvalM y) =
  EvalM
    ( \env -> case x env of
        Left _ -> y env
        Right a -> Right a
    )

eval :: Exp -> EvalM Val
eval (CstInt i) = return (ValInt i)
eval (CstBool b) = return (ValBool b)
eval (Add e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  case (v1, v2) of
    (ValInt i1, ValInt i2) -> return (ValInt (i1 + i2))
    _ -> failure "type error in addition"
eval (Sub e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  case (v1, v2) of
    (ValInt i1, ValInt i2) -> return (ValInt (i1 - i2))
    _ -> failure "type error in subtraction"
eval (Mul e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  case (v1, v2) of
    (ValInt i1, ValInt i2) -> return (ValInt (i1 * i2))
    _ -> failure "type error in multiplication"
eval (Div e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  case (v1, v2) of
    (ValInt i1, ValInt i2)
      | i2 > 0 -> return (ValInt (i1 `div` i2))
      | i2 == 0 -> failure "division by zero"
      | i2 < 0 -> failure "division by negative number"
    _ -> failure "type error in division"
eval (Pow e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  case (v1, v2) of
    (ValInt i1, ValInt i2)
      | i2 >= 0 -> return (ValInt (i1 ^ i2))
      | i2 < 0 -> failure "negative exponent"
    _ -> failure "type error in exponentiation"
eval (Eql e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  case (v1, v2) of
    (ValInt i1, ValInt i2) -> return (ValBool (i1 == i2))
    (ValBool b1, ValBool b2) -> return (ValBool (b1 == b2))
    _ -> failure "type error in equality"
eval (If e1 e2 e3) = do
  v1 <- eval e1
  case v1 of
    ValBool True -> eval e2
    ValBool False -> eval e3
    _ -> failure "type error in if"
eval (Var v) = do
  env <- askEnv
  case envLookup v env of
    Just val -> return val
    Nothing -> failure "unbound variable"
eval (Let v e1 e2) = do
  v1 <- eval e1
  localEnv (envExtend v v1) (eval e2)
eval (Lambda v e) = do
  env <- askEnv
  return (ValFun env v e)
eval (Apply e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  case v1 of
    ValFun env' v e -> localEnv (const (envExtend v v2 env')) (eval e)
    _ -> failure "type error in application"
eval (TryCatch e1 e2) = catch (eval e1) (eval e2)
