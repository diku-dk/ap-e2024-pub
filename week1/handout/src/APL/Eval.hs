module APL.Eval (
  eval,
  Val (..),
)
where

import APL.AST (Exp (..))

data Val
  = ValInt Integer
  deriving (Eq, Show)

type Error = String

evalIntOp :: (Integer -> Integer -> Either Error Integer) -> Exp -> Exp -> Either Error Val
evalIntOp f e1 e2 =
  case (eval e1, eval e2) of
    (Left err, _) -> Left err
    (_, Left err) -> Left err
    (Right (ValInt x), Right (ValInt y)) -> case f x y of
      Left err -> Left err
      Right z -> Right $ ValInt z

evalIntOp' :: (Integer -> Integer -> Integer) -> Exp -> Exp -> Either Error Val
evalIntOp' f =
  evalIntOp f'
 where
  f' x y = Right $ f x y

eval :: Exp -> Either Error Val
eval (CstInt x) = Right $ ValInt x
eval (Add x y) = evalIntOp' (+) x y
eval (Sub x y) = evalIntOp' (-) x y
eval (Mul x y) = evalIntOp' (*) x y
eval (Div x y) = evalIntOp checkDiv x y
 where
  checkDiv _ 0 = Left "Division By zero"
  checkDiv e1 e2 = Right $ e1 `div` e2
eval (Pow x y) = evalIntOp checkPow x y
 where
  checkPow e1 e2 =
    if e2 < 0
      then Left "Negative Exponent"
      else Right $ e1 ^ e2
