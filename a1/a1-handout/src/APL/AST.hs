module APL.AST
  ( VName,
    Exp (..),
    printExp,
  )
where

type VName = String

data Exp
  = CstInt Integer
  | CstBool Bool
  | Add Exp Exp
  | Sub Exp Exp
  | Mul Exp Exp
  | Div Exp Exp
  | Pow Exp Exp
  | Eql Exp Exp
  | If Exp Exp Exp
  | Var VName
  | Let VName Exp Exp
  | Lambda VName Exp
  | Apply Exp Exp
  -- TODO: add cases
  deriving (Eq, Show)

printExp :: Exp -> String
printExp = undefined -- TODO
