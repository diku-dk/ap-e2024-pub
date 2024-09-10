module APL.Eval
  (
  Val(..),
  eval,
  envEmpty
  )
where

import APL.AST (Exp(..), VName)
 
data Val = 
  ValInt Integer
  | ValBool Bool
  deriving (Eq, Show)

type Error = String

type Env = [(VName, Val)]

envEmpty :: Env
envEmpty = []

envExtend :: VName -> Val -> Env -> Env
envExtend name val env = (name, val) : env

envLookup :: VName -> Env -> Maybe Val
envLookup name env =
  case (env) of
  (n, v) : _ | n == name -> Just v
  _ -> Nothing

eval :: Env -> Exp -> Either Error Val

eval _ (CstInt x) = Right $ ValInt x

eval _ (CstBool b) = Right $ ValBool b

eval env (Eql left right) = 
    case (eval env left, eval env right) of
    (Left err, _) -> Left err
    (_, Left err) -> Left err
    (Right (ValBool lb), Right (ValBool rb)) -> Right (ValBool (lb == rb))
    (Right (ValInt li), Right (ValInt ri)) -> Right (ValBool (li == ri))
    _ -> Left "not supported"

eval env (If cond e1 e2) =
  case (eval env cond) of
    Left err -> Left err
    Right (ValBool True) -> eval env e1
    Right (ValBool False) -> eval env e2
    Right _ -> Left "not supported"

eval env (Var name) =
  case (envLookup name env) of
    Nothing -> Left ("variable " ++ name ++ " was not found")
    Just e -> Right e

eval env (Let name e1 e2) =
  case (eval env e1) of
    Left err -> Left err
    Right val -> 
        let newenv = envExtend name val env
        in eval newenv e2

eval env (Add x y) = 
    case (eval env x, eval env y) of
    (Left err, _) -> Left err
    (_, Left err) -> Left err
    (Right (ValBool _), _ ) -> Left "Add does not support Bool"
    (_, Right (ValBool _)) -> Left "Add does not support Bool"
    (Right (ValInt rx), Right (ValInt ry)) -> Right (ValInt (rx + ry))

eval env (Mul x y)  = 
    case (eval env x, eval env y) of
    (Left err, _) -> Left err
    (_, Left err) -> Left err
    (Right (ValBool _), _ ) -> Left "Mul does not support Bool"
    (_, Right (ValBool _)) -> Left "Mul does not support Bool"
    (Right (ValInt rx), Right (ValInt ry)) -> Right (ValInt (rx * ry))

eval env (Sub x y)  = 
    case (eval env x, eval env y) of
    (Left err, _) -> Left err
    (_, Left err) -> Left err
    (Right (ValBool _), _ ) -> Left "Sub does not support Bool"
    (_, Right (ValBool _)) -> Left "Sub does not support Bool"
    (Right (ValInt rx), Right (ValInt ry)) -> Right (ValInt (rx - ry))

eval env (Div x y)  = 
    case (eval env x, eval env y) of
    (Left err, _) -> Left err
    (_, Left err) -> Left err
    (Right (ValBool _), _ ) -> Left "Div does not support Bool"
    (_, Right (ValBool _)) -> Left "Div does not support Bool"
    (Right (ValInt rx), Right (ValInt ry)) -> Right (ValInt (rx `div` ry))

eval env (Pow x y)  = 
    case (eval env x, eval env y) of
    (Left err, _) -> Left err
    (_, Left err) -> Left err
    (Right (ValBool _), _ ) -> Left "Pow does not support Bool"
    (_, Right (ValBool _)) -> Left "Pow does not support Bool"
    (Right (ValInt rx), Right (ValInt ry)) -> Right (ValInt (rx ^ ry))

