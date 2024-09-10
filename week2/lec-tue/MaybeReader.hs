module MaybeReader where

newtype MaybeReader env a = MaybeReader (env -> Maybe a)

runMaybeReader :: env -> MaybeReader env a -> Maybe a
runMaybeReader e (MaybeReader f) = f e

instance Functor (MaybeReader env) where
    fmap = undefined

instance Applicative (MaybeReader env) where
    pure = undefined
    (<*>) = undefined

instance Monad (MaybeReader env) where
    (>>=) = undefined

data Country = Brazil | Cameroon | Denmark
    deriving Show

type Age = Integer

canVote :: Age -> MaybeReader Country Bool
canVote age = MaybeReader $ \country ->
    case country of
        Brazil -> Just $ age >= 16
        Cameroon -> Just $ age >= 20
        Denmark -> Just $ age >= 18

ask :: MaybeReader env env
ask = MaybeReader $ \env -> Just env

canVote' :: Age -> MaybeReader Country Bool
canVote' age = fmap f ask
    where
        f Brazil = age >= 16
        f Cameroon = age >= 20
        f Denmark = age >= 18
