module Generators where

import Test.QuickCheck

-- always x is x with 100% prob
always :: a -> Gen a
always x = pure x

