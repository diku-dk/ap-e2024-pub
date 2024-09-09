module Eq where

{-
class Eq a where
    (==) :: a -> a -> Bool
    (/=) :: a -> a -> Bool

Eq laws:
    Reflexivity:    x == x = True
    Symmetry:       x == y = y == x
    Transitivity:   if x == y && y == z = True, then x == z = True
    Extensionality: if x == y = True and f is a function whose return type is an instance of Eq, then f x == f y = `True`
    Negation:       x /= y = not (x == y).
 -}

data Pos1 = Pos1 Integer Integer
    deriving Show

instance Eq Pos1 where
    Pos1 x y == Pos1 x' y' = x == x' && y == y'
-- Lawful



data Pos2 = Pos2 Integer Integer
    deriving Show

instance Eq Pos2 where
    Pos2 x y == Pos2 x' y' = x == x' || y == y'
-- Lawless, violates transitivity



data Pos3 = Pos3 Integer Integer
    deriving Show

instance Eq Pos3 where
    Pos3 _ _ == Pos3 _ _ = True
-- Lawless, violates extensionality
oops :: Pos3 -> Integer
oops (Pos3 x _) = x



data Pos4 = Pos4 Integer Integer
    deriving Show

instance Eq Pos4 where
    Pos4 _ _ == Pos4 _ _ = False
-- Lawless, violates reflexivity



data Pos5 = Pos5 (Bool -> Integer)

instance Eq Pos5 where
    Pos5 f == Pos5 g = f False == g False && f True == g True
-- Lawful
