module Eq where

import Test.QuickCheck

{-
class Eq a where
    (==) :: a -> a -> Bool
    (/=) :: a -> a -> Bool

Eq laws:
    Reflexivity:    ∀x: x == x = True
    Symmetry:       x == y = y == x
    Transitivity:   if x == y && y == z = True, then x == z = True
    Extensionality: if x == y = True and f is a function whose return type is an instance of Eq, then f x == f y = `True`
    Negation:       x /= y = not (x == y).
 -}

-- length (xs ++ ys) = length xs + length ys

reflexivity :: Eq a => a -> Bool
reflexivity x = x == x

{-
foo = [reflexivity (Pos1 x y) | x <- [0 .. 10], y <- [0 .. 10]]
foo' = do
    x <- [0 .. 10]
    y <- [0 .. 10]
    pure (reflexivity (Pos1 x y))
-- ∀x: reflexivity x = True
-}

symmetry :: Eq a => a -> a -> Bool
symmetry x y = (x == y) == (y == x)

transitivity :: Eq a => a -> a -> a -> Bool
transitivity x y z = not (x == y && y == z) || x == z

transitivity' :: Eq a => a -> a -> a -> Property
transitivity' x y z = (x == y && y == z) ==> x == z

data Pos1 = Pos1 Integer Integer
    deriving Show

instance Eq Pos1 where
    Pos1 x y == Pos1 x' y' = x == x' && y == y'
-- Lawful

instance Arbitrary Pos1 where
    arbitrary = Pos1 <$> arbitrary <*> arbitrary



data Pos2 = Pos2 Integer Integer
    deriving Show

instance Eq Pos2 where
    Pos2 x y == Pos2 x' y' = x == x' || y == y'
-- Lawless, violates transitivity

instance Arbitrary Pos2 where
    arbitrary = Pos2 <$> arbitrary <*> arbitrary



data Pos3 = Pos3 Integer Integer
    deriving Show

instance Eq Pos3 where
    Pos3 _ _ == Pos3 _ _ = True
-- Lawless, violates extensionality

instance Arbitrary Pos3 where
    arbitrary = Pos3 <$> arbitrary <*> arbitrary



data Pos4 = Pos4 Integer Integer
    deriving Show

instance Eq Pos4 where
    Pos4 _ _ == Pos4 _ _ = False
-- Lawless, violates reflexivity

instance Arbitrary Pos4 where
    arbitrary = Pos4 <$> arbitrary <*> arbitrary



data Pos5 = Pos5 Integer Integer
    deriving Show

instance Eq Pos5 where
    Pos5 91 _ == _ = True
    Pos5 x y == Pos5 x' y' = x == x' && y == y'

instance Arbitrary Pos5 where
    -- arbitrary = Pos5 <$> arbitrary <*> arbitrary
    arbitrary = pos5Gen
    {-
    shrink (Pos5 x y) =
        [Pos5 x' y' | x' <- shrink x, y' <- shrink y]
        -}
    shrink (Pos5 x y) =
        [Pos5 x' y | x' <- shrink x] ++
        [Pos5 x y' | y' <- shrink y]

pos5Gen :: Gen Pos5
pos5Gen = do
    x <- frequency [(1, pure 91), (49, arbitrary)]
    y <- arbitrary
    pure (Pos5 x y)

pos5Tests :: [Property]
pos5Tests =
    [ property (reflexivity :: Pos5 -> Bool)
    , property (symmetry :: Pos5 -> Pos5 -> Bool)
    ]
