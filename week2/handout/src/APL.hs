module APL
  ( VName,
    Exp (..),
    Val (..),
    eval,
    runEval,
    Error,
  )
where

import Control.Monad (ap, liftM)

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
  | TryCatch Exp Exp
  deriving (Eq, Show)

data Val
  = ValInt Integer
  | ValBool Bool
  | ValFun Env VName Exp
  deriving (Eq, Show)

type Env = [(VName, Val)]

envEmpty :: Env
envEmpty = []

envExtend :: VName -> Val -> Env -> Env
envExtend v val env = (v, val) : env

envLookup :: VName -> Env -> Maybe Val
envLookup v env = lookup v env

type Error = String

newtype EvalM a = EvalM a -- TODO

runEval :: EvalM a -> Either Error a
runEval = undefined -- TODO

eval :: Exp -> EvalM Val
eval = undefined -- TODO
