module APL.Check (checkExp, Error) where

import APL.AST (Exp (..), VName)

type Error = String

newtype CheckM a = CheckM () -- TODO - give this a proper definition.

check :: Exp -> CheckM ()
check = undefined

checkExp :: Exp -> Maybe Error
checkExp = undefined
