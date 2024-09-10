module Sequence
    ( Sequence
    , single
    , fromList
    , toList
    , toMonoid
    )
where

data Sequence a = Empty | Single a | Append (Sequence a) (Sequence a)
{-
Invariants:
    Append Empty s = s
    Append s Empty = s
    Append (Append s1 s2) s3 = Append s1 (Append s2 s3) 
-}


instance Show a => Show (Sequence a) where
    show = show . toList

instance Eq a => Eq (Sequence a) where
    s1 == s2 = toList s1 == toList s2

instance Foldable Sequence where
    foldr f k Empty = k
    foldr f k (Single x) = f x k
    foldr f k (Append s1 s2) = foldr f (foldr f k s2) s1

-- Examples of foldr:
-- foldr f k [1, 2, 3] = f 1 (f 2 (f 3 k))
-- foldr (:) [] = id

instance Functor Sequence where
    fmap _ Empty = Empty
    fmap f (Single x) = Single (f x)
    fmap f (Append s1 s2) = Append (fmap f s1) (fmap f s2)

instance Semigroup (Sequence a) where
    s1 <> s2 = Append s1 s2

instance Monoid (Sequence a) where
    mempty = Empty

single :: a -> Sequence a
single = Single

fromList :: [a] -> Sequence a
fromList [] = Empty
fromList (x : xs) = Append (Single x) (fromList xs)

toList :: Sequence a -> [a]
toList = foldr (:) []

toMonoid :: Monoid b => (a -> b) -> Sequence a -> b
toMonoid _ Empty = mempty
toMonoid f (Single x) = f x
toMonoid f (Append s1 s2) = toMonoid f s1 <> toMonoid f s2
