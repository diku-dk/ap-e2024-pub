{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use tuple-section" #-}
module APL.Check (checkExp, Error) where

import APL.AST (Exp (..), VName)
import Control.Monad (ap, liftM)

type Error = String

type Env = [VName]

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

check :: Exp -> CheckM ()
check = undefined

checkExp :: Exp -> Maybe Error
checkExp = undefined
