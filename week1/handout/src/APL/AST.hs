module APL.AST (
  Exp (..),
)
where

data Exp
  = CstInt Integer
  deriving (Eq, Show)
