module APL.Eval (
  eval,
  Val (..),
)
where

import APL.AST (Exp (CstInt))

data Val
  = ValInt Integer
  deriving (Eq, Show)

eval :: Exp -> Val
eval (CstInt x) = ValInt x
