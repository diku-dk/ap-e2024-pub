module State where

import Prelude hiding (mapM)

data Tree = Leaf | Branch Tree Integer Tree
    deriving Show

ex1 :: Tree
ex1 = Branch (Branch Leaf 33 (Branch Leaf 55 Leaf)) 77 (Branch Leaf 99 Leaf)

renumber :: Tree -> Tree
renumber tree = fst $ go tree 0
    where
        go :: Tree -> Integer -> (Tree, Integer)
        go Leaf n = (Leaf, n)
        go (Branch t1 _ t2) n =
            let (t1', n') = go t1 n
                (t2', n'') = go t2 (n' + 1)
            in (Branch t1' n' t2', n'')



newtype State s a = State (s -> (a, s))

runState :: s -> State s a -> (a, s)
runState s (State t) = t s

get :: State s s
get = State $ \s -> (s, s)

put :: s -> State s ()
put s = State $ \_ -> ((), s)

instance Functor (State s) where
    -- fmap f x = x >>= \x' -> pure (f x')
    fmap f (State t) = State $ \s ->
        let (x, s') = t s
        in (f x, s')
    -- f :: a -> b
    -- x :: a
    -- t :: s -> (a, s)

instance Applicative (State s) where
    pure x = State $ \s -> (x, s)
    -- (<*>) :: State s (a -> b) -> State s a -> State s b
    State tf <*> State tx = State $ \s ->
        let (f, s') = tf s
            (x, s'') = tx s'
        in (f x, s'')

instance Monad (State s) where
    -- (>>=) :: State s a -> (a -> State s b) -> State s b
    State tx >>= f = State $ \s ->
        let (x, s') = tx s
            State ty = f x
            (y, s'') = ty s'
        in (y, s'')

renumber' :: Tree -> Tree
renumber' tree = fst $ runState 0 (renumberS tree)

increment :: State Integer Integer
increment = get >>= \n -> put (n + 1) >>= \() -> pure n

renumberS :: Tree -> State Integer Tree
renumberS Leaf = pure Leaf
renumberS (Branch t1 _ t2) =
    renumberS t1 >>= \t1' ->
    increment >>= \n ->
    renumberS t2 >>= \t2' ->
    pure $ Branch t1' n t2'

renumber'' :: Tree -> Tree
renumber'' tree = fst $ runState 0 (renumberS' tree)

renumberS' :: Tree -> State Integer Tree
renumberS' Leaf = pure Leaf
renumberS' (Branch t1 _ t2) = do
    t1' <- renumberS' t1
    n <- increment
    t2' <- renumberS' t2
    pure $ Branch t1' n t2'
 
renumberS'' :: Tree -> State Integer Tree
renumberS'' Leaf = pure Leaf
renumberS'' (Branch t1 _ t2) =
    Branch <$> renumberS'' t1 <*> increment <*> renumberS'' t2


-- map :: (a -> b) -> [a] -> [b]
-- map :: (a -> m b) -> [a] -> [m b]
mapM :: Monad m => (a -> m b) -> [a] -> m [b]
mapM f [] = pure []
mapM f (x : xs) = do
    y <- f x
    ys <- mapM f xs
    pure (y : ys)

renumberMany' :: [Tree] -> [Tree]
renumberMany' trees = map renumber trees

renumberMany :: [Tree] -> [Tree]
renumberMany trees = fst . runState 0 $ mapM renumberS trees
-- fst . runState 0 $ map renumberS trees
