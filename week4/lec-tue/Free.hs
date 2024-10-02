import Control.Monad (ap, liftM)

data Pair a = Pair a a

data Triple a = Triple a a a

data Tree f a
  = Leaf a
  | Inner (f (Tree f a))

type TreePair = Tree Pair

type TreeTriple = Tree Triple

tree :: Tree Triple Bool
tree =
  Inner
    ( Triple
        (Leaf True)
        (Leaf True)
        ( Inner
            ( Triple
                (Leaf True)
                (Leaf True)
                (Leaf True)
            )
        )
    )

data Free f a
  = Pure a
  | Free (f (Free f a))

instance (Functor f) => Functor (Free f) where
  fmap = liftM

instance (Functor f) => Applicative (Free f) where
  (<*>) = ap
  pure x = Pure x

instance (Functor f) => Monad (Free f) where
  Free x >>= y = Free $ h <$> x
    where
      h x' = x' >>= y
  Pure x >>= y = y x

newtype ReaderOp env a
  = ReaderOp (env -> a)

instance Functor (ReaderOp env) where
  fmap f (ReaderOp g) =
    ReaderOp $ \env -> f (g env)

type Reader env a = Free (ReaderOp env) a

ask :: Reader env env
ask = Free $ ReaderOp $ \env -> Pure env

foo :: Reader Int Bool
foo = do
  x <- ask
  y <- ask
  pure (x == y)

runReader :: env -> Reader env a -> a
runReader _ (Pure x) = x
runReader env (Free (ReaderOp x)) =
  runReader env $ x env

flipReader :: (env, env) -> Reader env a -> a
flipReader _ (Pure x) = x
flipReader (env1, env2) (Free (ReaderOp x)) =
  flipReader (env2, env1) $ x env1

data StateOp s a
  = StatePut s a
  | StateGet (s -> a)

instance Functor (StateOp s) where
  fmap f (StatePut s c) =
    StatePut s (f c)
  fmap f (StateGet c) =
    StateGet $ \s -> f (c s)

type State s a = Free (StateOp s) a

get :: State s s
get = Free $ StateGet $ \s -> Pure s

put :: s -> State s ()
put s = Free $ StatePut s $ Pure ()

runState :: s -> State s a -> a
runState _ (Pure x) = x
runState s (Free (StateGet c)) =
  runState s (c s)
runState _ (Free (StatePut s c)) =
  runState s c

data FibOp a
  = FibLog String a
  | FibMemo Int (FibM Int) (Int -> a)

instance Functor FibOp where
  fmap f (FibLog s x) =
    FibLog s $ f x
  fmap f (FibMemo n fibn c) =
    FibMemo n fibn $ \x -> f (c x)

type FibM a = Free FibOp a

fibLog :: String -> FibM ()
fibLog s = Free $ FibLog s $ Pure ()

memo :: Int -> FibM Int -> FibM Int
memo n fibn =
  Free $ FibMemo n fibn $ \x -> Pure x

fib :: Int -> FibM Int
fib 0 = pure 1
fib 1 = pure 1
fib n = memo n $ do
  fibLog $ "f(" ++ show n ++ ")"
  x <- fib (n - 1)
  y <- fib (n - 2)
  pure $ x + y

pureRunFibM :: FibM a -> a
pureRunFibM (Pure x) = x
pureRunFibM (Free (FibLog _ c)) =
  pureRunFibM c
pureRunFibM (Free (FibMemo _ fibn c)) =
  pureRunFibM $ c $ pureRunFibM fibn

ioRunFibM :: FibM a -> IO a
ioRunFibM (Pure x) = pure x
ioRunFibM (Free (FibLog s c)) = do
  putStrLn s
  ioRunFibM c
ioRunFibM _ = undefined

listRunFibM :: FibM a -> ([String], a)
listRunFibM (Pure x) = ([], x)
listRunFibM (Free (FibLog s c)) =
  let (l, x) = listRunFibM c
   in (s : l, x)
listRunFibM _ = undefined

memoRunFibM :: FibM a -> IO a
memoRunFibM m = fst <$> run [] m
  where
    run ::
      [(Int, Int)] ->
      FibM a ->
      IO (a, [(Int, Int)])
    run cache (Pure x) = pure (x, cache)
    run cache (Free (FibLog s c)) = do
      putStrLn s
      run cache c
    run cache (Free (FibMemo n fibn c)) =
      case lookup n cache of
        Just x -> run cache $ c x
        Nothing -> do
          (x, cache') <- run cache fibn
          run ((n, x) : cache') $ c x
