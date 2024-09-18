module APL.Check (checkExp, Error) where

import APL.AST (Exp (..), VName)
import Control.Monad (ap, liftM)

type Error = String

newtype CheckM a = CheckM ([VName] -> VName -> ([VName], Maybe a))

instance Functor CheckM where
  fmap = liftM

instance Applicative CheckM where
  pure x = CheckM $ \env _name -> (env, Just x)
  (<*>) = ap

instance Monad CheckM where
  CheckM x >>= f = undefined

check :: Exp -> CheckM ()
check = undefined

checkExp :: Exp -> Maybe Error
checkExp = undefined
