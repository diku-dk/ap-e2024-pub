module Recursion where

import Control.Applicative (liftA)
import Test.QuickCheck

data Exp
    = Abort
    | Const Bool
    | Not Exp
    | And Exp Exp
    | Or Exp Exp
    | Xor Exp Exp
    deriving (Show, Eq)

genExp :: Int -> Gen Exp
genExp n = frequency $
    [ (10, pure Abort) | n <= 1] ++
    [ (100, Const <$> arbitrary) | n <= 1] ++
    [ (100, Not <$> genExp (n - 1)) | n >= 2] ++
    [ (100, do
        n1 <- chooseInt (1, n - 2)
        let n2 = n - 1 - n1
        And <$> genExp n1 <*> genExp n2) | n >= 3]

instance Arbitrary Exp where
    arbitrary = sized $ \size ->
        chooseInt (1, size) >>= genExp

{-
    arbitrary = frequency
        [ (100, pure Abort)
        , (100, Const <$> arbitrary)
        , (100, Not <$> arbitrary)
        , (60, And <$> arbitrary <*> arbitrary)
        , (60, Or <$> arbitrary <*> arbitrary)
        , (60, Xor <$> arbitrary <*> arbitrary)
        ]
-}

    shrink Abort = []
    shrink (Const _) = []
    shrink (Not e) = e : [Not e' | e' <- shrink e]
    shrink (And e1 e2) = e1 : e2 :
        [And e1' e2 | e1' <- shrink e1] ++
        [And e1 e2' | e2' <- shrink e2]
    shrink _ = []
{-
P = 0.25 + 0.25 + 0.25 P + 0.25 P², P = 1
P = 0.2 + 0.2 + 0.2 P + 0.4 P², P = 1 (with Or)
P = 1/6 + 1/6 + 1/6 P + 3/6 P², P = 2/3 (with Or and Xor)
 -}

expSize :: Exp -> Int
expSize Abort = 1
expSize (Const _) = 1
expSize (Not e) = expSize e + 1
expSize (And e1 e2) = expSize e1 + expSize e2 + 1
expSize (Or e1 e2) = expSize e1 + expSize e2 + 1
expSize (Xor e1 e2) = expSize e1 + expSize e2 + 1

data Eval a = Error | Result a
    deriving (Show, Eq)

instance Functor Eval where
    fmap = liftA

instance Applicative Eval where
    pure = Result
    Error <*> Error = Error
    Error <*> Result _ = Error
    Result _ <*> Error = Error
    Result f <*> Result x = Result (f x)

xor :: Bool -> Bool -> Bool
xor True = not
xor False = id

eval :: Exp -> Eval Bool
eval Abort = Error
eval (Const b) = pure b
eval (Not e) = not <$> eval e
eval (And e1 e2) = (&&) <$> eval e1 <*> eval e2
eval (Or e1 e2) = (||) <$> eval e1 <*> eval e2
eval (Xor e1 e2) = xor <$> eval e1 <*> eval e2

optimise :: Exp -> Exp
optimise (Not e) = Not (optimise e)
optimise (And e1 e2)
    | e1' == e2' = e1'
    | e1' == Not e2' = Const False
    | e2' == Not e1' = Const False
    | otherwise = And e1' e2'
    where
        e1' = optimise e1
        e2' = optimise e2
optimise (Or e1 e2) = Or (optimise e1) (optimise e2)
optimise (Xor e1 e2) = Xor (optimise e1) (optimise e2)
optimise e = e

prop_optimise :: Exp -> Bool
prop_optimise e = eval e == eval (optimise e)

notTooManyErrors :: Exp -> Property
notTooManyErrors e = cover 50 (eval e /= Error) "not too many errors" ()
