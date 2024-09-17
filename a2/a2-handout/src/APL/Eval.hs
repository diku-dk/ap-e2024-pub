module APL.Eval
  ( Val (..),
    eval,
    runEval,
    Error,
  )
where

import APL.AST (Exp (..), VName)
import Control.Monad (ap, liftM)

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

type State = ([String], [(Val, Val)])

stateEmpty :: State
stateEmpty = ([], [])

newtype EvalM a = EvalM (Env -> State -> (State, Either Error a))

instance Functor EvalM where
  fmap = liftM

instance Applicative EvalM where
  pure x = EvalM $ \_env state -> (state, Right x)
  (<*>) = ap

instance Monad EvalM where
  EvalM x >>= f = EvalM $ \env state ->
    case x env state of
      (state', Left err) -> (state', Left err)
      (state', Right x') ->
        let EvalM y = f x'
         in y env state'

askEnv :: EvalM Env
askEnv = EvalM $ \env state -> (state, Right env)

localEnv :: (Env -> Env) -> EvalM a -> EvalM a
localEnv f (EvalM m) = EvalM $ \env state -> m (f env) state

failure :: String -> EvalM a
failure s = EvalM $ \_env state -> (state, Left s)

catch :: EvalM a -> EvalM a -> EvalM a
catch (EvalM m1) (EvalM m2) = EvalM $ \env (state, store) ->
  case m1 env (state, store) of
    ((state', _), Left _) -> m2 env (state', store)
    (state', Right x) -> (state', Right x)

runEval :: EvalM a -> ([String], Either Error a)
runEval (EvalM m) =
  let result = m envEmpty stateEmpty
   in case result of
        ((state, _), Left err) -> (state, Left err)
        ((state, _), Right x) -> (state, Right x)

evalIntBinOp :: (Integer -> Integer -> EvalM Integer) -> Exp -> Exp -> EvalM Val
evalIntBinOp f e1 e2 = do
  v1 <- eval e1
  v2 <- eval e2
  case (v1, v2) of
    (ValInt x, ValInt y) -> ValInt <$> f x y
    (_, _) -> failure "Non-integer operand"

evalIntBinOp' :: (Integer -> Integer -> Integer) -> Exp -> Exp -> EvalM Val
evalIntBinOp' f e1 e2 =
  evalIntBinOp f' e1 e2
  where
    f' x y = pure $ f x y

eval :: Exp -> EvalM Val
eval (CstInt x) = pure $ ValInt x
eval (CstBool b) = pure $ ValBool b
eval (Var v) = do
  env <- askEnv
  case envLookup v env of
    Just x -> pure x
    Nothing -> failure $ "Unknown variable: " ++ v
eval (Add e1 e2) = evalIntBinOp' (+) e1 e2
eval (Sub e1 e2) = evalIntBinOp' (-) e1 e2
eval (Mul e1 e2) = evalIntBinOp' (*) e1 e2
eval (Div e1 e2) = evalIntBinOp checkedDiv e1 e2
  where
    checkedDiv _ 0 = failure "Division by zero"
    checkedDiv x y = pure $ x `div` y
eval (Pow e1 e2) = evalIntBinOp checkedPow e1 e2
  where
    checkedPow x y =
      if y < 0
        then failure "Negative exponent"
        else pure $ x ^ y
eval (Eql e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  case (v1, v2) of
    (ValInt x, ValInt y) -> pure $ ValBool $ x == y
    (ValBool x, ValBool y) -> pure $ ValBool $ x == y
    (_, _) -> failure "Invalid operands to equality"
eval (If cond e1 e2) = do
  cond' <- eval cond
  case cond' of
    ValBool True -> eval e1
    ValBool False -> eval e2
    _ -> failure "Non-boolean conditional."
eval (Let var e1 e2) = do
  v1 <- eval e1
  localEnv (envExtend var v1) $ eval e2
eval (Lambda var body) = do
  env <- askEnv
  pure $ ValFun env var body
eval (Apply e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  case (v1, v2) of
    (ValFun f_env var body, arg) ->
      localEnv (const $ envExtend var arg f_env) $ eval body
    (_, _) ->
      failure "Cannot apply non-function"
eval (TryCatch e1 e2) =
  eval e1 `catch` eval e2
eval (Print s e) = do
  v <- eval e
  case v of
    ValFun {} -> evalPrint (s ++ ": #<fun>")
    ValInt x -> evalPrint (s ++ ": " ++ show x)
    ValBool b -> evalPrint (s ++ ": " ++ show b)
  EvalM $ \_env state -> (state, Right v)
eval (KvPut e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  evalKvPut v1 v2
  EvalM $ \_env state -> (state, Right v2)
eval (KvGet e) = do
  v <- eval e
  evalKvGet v

evalPrint :: String -> EvalM ()
evalPrint s = EvalM $ \_env (state, store) -> ((state ++ [s], store), Right ())

evalKvGet :: Val -> EvalM Val
evalKvGet v = EvalM $ \_env (state, store) ->
  case lookup v store of
    Just x -> ((state, store), Right x)
    Nothing -> ((state, store), Left $ "Invalid key: " ++ show v)

evalKvPut :: Val -> Val -> EvalM ()
evalKvPut k v = EvalM $ \_env (state, store) ->
  case lookup k store of
    Just _ ->
      let store' = map (\(k', v') -> if k' == k then (k', v) else (k', v')) store
       in ((state, store'), Right ())
    Nothing -> ((state, store ++ [(k, v)]), Right ())
