module Functor where

import Prelude hiding (Maybe (..))

{-
class Functor f where
    fmap :: (a -> b) -> (f a -> f b)

Functor laws:
    Identity:    fmap id = id
    Composition: fmap (f . g) = fmap f . fmap g
 -}

data Wrap a = Wrap a
    deriving Show

instance Functor Wrap where
    fmap f (Wrap x) = Wrap (f x)
-- Lawful



data Maybe a = Nothing | Just a
    deriving Show

instance Functor Maybe where
    fmap _ Nothing = Nothing
    fmap f (Just x) = Just (f x)
-- Lawful



data List a = Nil | Cons a (List a)
    deriving Show

instance Functor List where
    fmap f Nil = Nil
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)
-- Lawful



data Pair a = Pair a a
    deriving Show

instance Functor Pair where
    fmap f (Pair x y) = Pair (f y) (f x)
-- Lawless, violates identity



-- Beware: you are not expected to understand this
data Cont k a = Cont ((a -> k) -> k)

instance Functor (Cont k) where
    fmap f (Cont c) = Cont (\g -> c (\x -> g (f x)))
    -- or: fmap f (Cont c) = Cont (c . (. f))
-- Lawful



data Contra k a = Contra (a -> k)

instance Functor (Contra k) where
    fmap f (Contra c) = Contra (\x -> undefined)
-- No possible lawful instance
