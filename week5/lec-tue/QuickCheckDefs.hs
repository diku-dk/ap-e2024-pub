module QuickCheckDefs where

data Gen a {- opaque -}
data Property {- opaque -}

class Arbitrary a where
    arbitrary :: Gen a
    shrink :: a -> [a]

class Testable prop where
    property :: prop -> Property

elements :: [a] -> Gen a
oneof :: [Gen a] -> Gen a
frequency :: [(Int, Gen a)] -> Gen a
chooseInteger :: (Integer, Integer) -> Gen Int
suchThat :: Gen a -> (a -> Bool) -> Gen a
