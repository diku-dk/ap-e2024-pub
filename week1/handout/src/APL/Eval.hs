module APL.Eval (

)
where

import APL.AST ()

data Val
  = ValInt Integer
  deriving (Eq, Show)
