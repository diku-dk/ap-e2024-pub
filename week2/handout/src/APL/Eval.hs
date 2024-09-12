module APL.Eval (
  Val (..),
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

newtype EvalM a = EvalM (Either Error a)

instance Functor EvalM where
  fmap = liftM

instance Applicative EvalM where
  pure x = EvalM $ Right x
  (<*>) = ap

instance Monad EvalM where
  EvalM x >>= f = EvalM $ case x of
    Left err -> Left err
    Right x' ->
      let EvalM y = f x'
       in y

failure :: String -> EvalM a
failure s = EvalM $ Left s

runEval :: EvalM a -> Either Error a
runEval (EvalM x) = x

bIntOp :: (Integer -> Integer -> Integer) -> Env -> Exp -> Exp -> EvalM Val
bIntOp f env e1 e2 =
  do
    x <- eval env e1
    y <- eval env e2
    case (x, y) of
      (ValInt x', ValInt y') -> return $ ValInt $ f x' y'
      _ -> failure "non-Integer operand"
bBoolOp :: (Integer -> Integer -> Bool) -> Env -> Exp -> Exp -> EvalM Val
bBoolOp f env e1 e2 =
  do
    x <- eval env e1
    y <- eval env e2
    case (x, y) of
      (ValInt x', ValInt y') -> return $ ValBool $ f x' y'
      _ -> failure "non-Integer operand"

eval :: Env -> Exp -> EvalM Val
eval _ (CstInt x) = pure (ValInt x)
eval _ (CstBool x) = pure (ValBool x)
eval env (Add e1 e2) =
  bIntOp (+) env e1 e2
eval env (Sub e1 e2) =
  bIntOp (-) env e1 e2
eval env (Mul e1 e2) =
  bIntOp (*) env e1 e2
eval env (Pow e1 e2) =
  bIntOp (^) env e1 e2
eval env (Eql e1 e2) =
  bBoolOp (==) env e1 e2
eval env (If e1 e2 e3) =
  do
    x <- eval env e1
    case x of
      ValBool True ->
        eval env e2
      ValBool False ->
        eval env e3
      _ -> failure "Non-bool condition"
eval env (Var v) = do
  case envLookup v env of
    Just x -> return x
    Nothing -> failure $ "Unknown variable: " ++ v
eval env (Let v e1 e2) =
  do
    x <- eval env e1
    eval (envExtend v x env) e2
