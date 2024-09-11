module Reader where

newtype Reader env a = Reader (env -> a)

runReader :: env -> Reader env a -> a
runReader env (Reader f) = f env

data Country = Brazil | Cameroon | Denmark
    deriving Show

type Age = Integer

canVote :: Age -> Reader Country Bool
canVote age = Reader $ \country ->
    case country of
        Brazil -> age >= 16
        Cameroon -> age >= 20
        Denmark -> age >= 18

cannotVote :: Age -> Reader Country Bool
cannotVote age = Reader $ \country -> not (test country)
    where
        Reader test = canVote age

canBothVote :: Age -> Age -> Reader Country Bool
canBothVote age1 age2 = Reader $ \country -> test1 country && test2 country
    where
        Reader test1 = canVote age1
        Reader test2 = canVote age2



readerMap :: (a -> b) -> Reader env a -> Reader env b
readerMap f (Reader r) = Reader (\e -> f (r e))

cannotVote' :: Age -> Reader Country Bool
cannotVote' age = readerMap not (canVote age)

readerMap2 :: (a -> b -> c) -> Reader env a -> Reader env b -> Reader env c
readerMap2 f (Reader r1) (Reader r2) = Reader (\e -> f (r1 e) (r2 e))

canBothVote' :: Age -> Age -> Reader Country Bool
canBothVote' age1 age2 = readerMap2 (&&) (canVote age1) (canVote age2)



constant :: a -> Reader env a
constant x = Reader (\_ -> x)

apply :: Reader env (a -> b) -> Reader env a -> Reader env b
apply (Reader f) (Reader x) = Reader (\e -> f e (x e))

cannotVote'' :: Age -> Reader Country Bool
cannotVote'' age = constant not `apply` canVote age

canBothVote'' :: Age -> Age -> Reader Country Bool
canBothVote'' age1 age2 =
    constant (&&) `apply` canVote age1 `apply` canVote age2



-- instances
instance Functor (Reader env) where
    fmap = readerMap

instance Applicative (Reader env) where
    pure = constant
    (<*>) = apply



people :: Reader Country [Age]
people = Reader $ \country ->
    case country of
        Brazil -> [4, 8, 15, 16, 23, 42]
        Cameroon -> [1, 2, 3, 5, 8, 13, 21, 34, 55]
        Denmark -> [1 .. 100]

numberOfVoters :: Reader Country Int
numberOfVoters = Reader $ \country ->
    let ppl = runReader country people
    in length $ filter (\age -> runReader country (canVote age)) ppl



readerBind :: Reader env a -> (a -> Reader env b) -> Reader env b
readerBind (Reader f) g = Reader $ \env -> runReader env (g (f env))

numberOfVoters' :: Reader Country Int
numberOfVoters' = undefined
