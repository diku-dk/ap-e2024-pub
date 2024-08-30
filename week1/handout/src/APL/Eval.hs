module APL.Eval
  ( eval,
    Val (..),
    Env,
    envEmpty,
  )
where

import APL.AST (Exp (..), VName)

data Val
  = ValInt Integer
  | ValBool Bool
  deriving (Eq, Show)

type Error = String

type Env = [(VName, Val)]

envEmpty :: Env
envEmpty = []

envExtend :: VName -> Val -> Env -> Env
envExtend x v env = (x, v) : env

envLookup :: VName -> Env -> Maybe Val
envLookup x env = lookup x env

eval :: Env -> Exp -> Either Error Val
eval _ (CstInt i) = Right $ ValInt i
eval _ (CstBool b) = Right $ ValBool b
eval env (Add e1 e2) = case (eval env e1, eval env e2) of
  (Right (ValInt i1), Right (ValInt i2)) -> Right $ ValInt $ i1 + i2
  (Left e, _) -> Left e
  (_, Left e) -> Left e
  _ -> Left "Type error"
eval env (Sub e1 e2) = case (eval env e1, eval env e2) of
  (Right (ValInt i1), Right (ValInt i2)) -> Right $ ValInt $ i1 - i2
  (Left e, _) -> Left e
  (_, Left e) -> Left e
  _ -> Left "Type error"
eval env (Mul e1 e2) = case (eval env e1, eval env e2) of
  (Right (ValInt i1), Right (ValInt i2)) -> Right $ ValInt $ i1 * i2
  (Left e, _) -> Left e
  (_, Left e) -> Left e
  _ -> Left "Type error"
eval env (Div e1 e2) = case (eval env e1, eval env e2) of
  (Right (ValInt i1), Right (ValInt i2)) ->
    if i2 == 0
      then Left "Division by zero"
      else Right $ ValInt $ i1 `div` i2
  (Left e, _) -> Left e
  (_, Left e) -> Left e
  _ -> Left "Type error"
eval env (Pow e1 e2) = case (eval env e1, eval env e2) of
  (Right (ValInt i1), Right (ValInt i2)) ->
    if i2 < 0
      then Left "Negative exponent"
      else Right $ ValInt $ i1 ^ i2
  (Left e, _) -> Left e
  (_, Left e) -> Left e
  _ -> Left "Type error"
eval env (Eql e1 e2) = case (eval env e1, eval env e2) of
  (Right (ValInt i1), Right (ValInt i2)) -> Right $ ValBool $ i1 == i2
  (Left e, _) -> Left e
  (_, Left e) -> Left e
  _ -> Left "Type error"
eval env (If e1 e2 e3) = case eval env e1 of
  Right (ValBool b) -> if b then eval env e2 else eval env e3
  Right _ -> Left "Type error"
  Left e -> Left e
eval env (Var x) = case envLookup x env of
  Just v -> Right v
  Nothing -> Left "Variable not found"
eval env (Let x e1 e2) = case eval env e1 of
  Right v -> eval (envExtend x v env) e2
  Left e -> Left e
