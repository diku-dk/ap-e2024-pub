module APL.Check (checkExp, Error) where

import APL.AST (Exp (..), VName)
import Control.Monad (ap, liftM)

type Error = String

type Env = [VName]
envEmpty :: Env
envEmpty = []

newtype CheckM a = CheckM (Env -> (Env, Either Error a))

instance Functor CheckM where
  fmap = liftM

instance Applicative CheckM where
  pure x = CheckM $ \env -> (env, Right x)
  (<*>) = ap

instance Monad CheckM where
  {- x :: (Env -> (Env, Either Error a))
  - f :: (a -> CheckM b)
  -  b :: (Env -> (Env, Either Error a))
  -}
  CheckM x >>= f = CheckM $ \env ->
    case x env of
      (env', Left err) -> (env', Left err)
      (env', Right a) ->
        let CheckM b = f a
         in b env'

check2Exp :: Exp -> Exp -> CheckM ()
check2Exp e1 e2 =
  do
    _ <- check e1
    _ <- check e2
    pure ()

check :: Exp -> CheckM ()
check e =
  case e of
    (CstInt _) -> pure ()
    (CstBool _) -> pure ()
    (Add e1 e2) -> check2Exp e1 e2
    (Sub e1 e2) -> check2Exp e1 e2
    (Mul e1 e2) -> check2Exp e1 e2
    (Div e1 e2) -> check2Exp e1 e2
    (Pow e1 e2) -> check2Exp e1 e2
    (Eql e1 e2) -> check2Exp e1 e2
    (If e1 e2 e3) -> undefined

runCheck :: CheckM a -> (Env, Either Error a)
runCheck (CheckM m) = m envEmpty

checkExp :: Exp -> (Env, Either Error ())
checkExp = runCheck . check
