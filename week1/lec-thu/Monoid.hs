module Monoid where

{-
class Semigroup a where
    (<>) :: a -> a -> a

Semigroup laws:
    Associativity:  (x <> y) <> z = x <> (y <> z)

class Semigroup a => Monoid a where
    mempty :: a

Monoid laws:
    Left identity:  x <> mempty = x
    Right identity: mempty <> x = x
 -}

{-
Given that a is an instance of Semigroup and b is an instance of Semigroup
then (a, b) is an instance of Semigroup
instance (Semigroup a, Semigroup b) => Semigroup (a, b) where
    -- (<>) :: (a, b) -> (a, b) -> (a, b)
    (x1, y1) <> (x2, y2) = (x1 <> x2, y1 <> y2)
-}

newtype Add = Add Integer
    deriving Show

instance Semigroup Add where
    Add m <> Add n = Add (m + n)

instance Monoid Add where
    mempty = Add 0
-- Lawful



newtype Diff = Diff Integer
    deriving Show

instance Semigroup Diff where
    Diff m <> Diff n = Diff (abs (m - n))

instance Monoid Diff where
    mempty = Diff 0
-- Lawless, violates associativity



newtype Mul = Mul Integer
    deriving Show

instance Semigroup Mul where
    Mul m <> Mul n = Mul (m * n)

instance Monoid Mul where
    mempty = Mul 1
-- Lawful



newtype Fun a = Fun (a -> a)

instance Semigroup (Fun a) where
    Fun f <> Fun g = Fun (f . g)

instance Monoid (Fun a) where
    mempty = Fun id
-- Lawful



newtype Reverse a = Reverse a
    deriving Show

instance Semigroup a => Semigroup (Reverse a) where
    Reverse x <> Reverse y = Reverse (y <> x)

instance Monoid a => Monoid (Reverse a) where
    mempty = Reverse mempty
-- Lawful
