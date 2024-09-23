module APL.Eval (
  Val (..),
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
envLookup = lookup

type State = ([String], [(Val, Val)])

stateEmpty :: State
stateEmpty = ([], [])

type Error = String

newtype EvalM a = EvalM (Env -> State -> (State, Either Error a))

instance Functor EvalM where
  fmap = liftM

instance Applicative EvalM where
  pure x = EvalM $ \_env s -> (s, Right x)
  (<*>) = ap

instance Monad EvalM where
  EvalM x >>= f = EvalM $ \env s ->
    case x env s of
      (s', Left err) -> (s', Left err)
      (s', Right x') ->
        let EvalM y = f x'
         in y env s'

askEnv :: EvalM Env
askEnv = EvalM $ \env s -> (s, Right env)

localEnv :: (Env -> Env) -> EvalM a -> EvalM a
localEnv f (EvalM m) = EvalM $ \env -> m (f env)

failure :: String -> EvalM a
failure s = EvalM $ \_env state -> (state, Left s)

evalKvGet :: Val -> EvalM Val
evalKvGet v = EvalM $ \_env (str, hash) ->
  case lookup v hash of
    Nothing -> ((str, hash), Left $ "Invalid key: " ++ show v)
    Just b -> ((str, hash), Right b)

evalKvPut :: Val -> Val -> EvalM ()
evalKvPut v1 v2 = EvalM $ \_env (str, hash) ->
  case lookup v1 hash of
    Nothing -> ((str, (v1, v2) : hash), Right ())
    Just _ ->
      ((str, (v1, v2) : hash2), Right ())
     where
      -- We remove the item, where the key is with filter.
      hash2 = filter (\(k, _) -> k /= v1) hash

evalPrint :: String -> EvalM ()
evalPrint s = EvalM $ \_env state ->
  case state of
    (str, hash) -> ((str ++ [s], hash), Right ())

catch :: EvalM a -> EvalM a -> EvalM a
catch (EvalM m1) (EvalM m2) = EvalM $ \env s ->
  case m1 env s of
    (_, Left _) -> m2 env s
    (s', Right x) -> (s', Right x)

runEval :: EvalM a -> ([String], Either Error a)
runEval (EvalM m) =
  case m envEmpty stateEmpty of
    ((str, _), z) -> (str, z)

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
eval (Print s e) =
  do
    v1 <- eval e
    case v1 of
      ValInt a ->
        do
          _ <- evalPrint (s ++ ": " ++ show a)
          pure $ ValInt a
      ValBool a ->
        do
          _ <- evalPrint (s ++ ": " ++ show a)
          pure $ ValBool a
      ValFun env v b ->
        do
          _ <- evalPrint (s ++ ": " ++ "#<fun>")
          pure $ ValFun env v b
eval (KvPut e1 e2) =
  do
    k <- eval e1
    v <- eval e2
    _ <- evalKvPut k v
    pure v
eval (KvGet e) =
  do
    v1 <- eval e
    evalKvGet v1
