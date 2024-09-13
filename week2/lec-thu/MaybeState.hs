module MaybeState where

newtype Tentative s a = Ten (s -> Maybe (a, s))

newtype Definite s a = Def (s -> (Maybe a, s))

runTentative :: s -> Tentative s a -> Maybe (a, s)
runTentative s (Ten t) = t s

runDefinite :: s -> Definite s a -> (Maybe a, s)
runDefinite s (Def t) = t s



instance Functor (Tentative s) where
    fmap f (Ten t) = Ten $ \s ->
        case t s of
            Nothing -> Nothing
            Just (x, s') -> Just (f x, s')

instance Functor (Definite s) where
    fmap f (Def t) = Def $ \s ->
        case t s of
            (Nothing, s') -> (Nothing, s')
            (Just x, s') -> (Just (f x), s')



instance Applicative (Tentative s) where
    pure x = Ten $ \s -> Just (x, s)
    Ten tf <*> Ten tx = Ten $ \s ->
        case tf s of
            Nothing -> Nothing
            Just (f, s') ->
                case tx s' of
                    Nothing -> Nothing
                    Just (x, s'') -> pure (f x, s'')

instance Applicative (Definite s) where
    pure x = Def $ \s -> (Just x, s)
    Def tf <*> Def tx = Def $ \s ->
        case tf s of
            (Nothing, s') -> (Nothing, s')
            (Just f, s') ->
                case tx s' of
                    (Nothing, s'') -> (Nothing, s'')
                    (Just x, s'') -> (Just (f x), s'')



instance Monad (Tentative s) where
    Ten tx >>= f = Ten $ \s ->
        case tx s of
            Nothing -> Nothing
            Just (x, s') -> runTentative s' (f x)

instance Monad (Definite s) where
    Def tx >>= f = Def $ \s ->
        case tx s of
            (Nothing, s') -> (Nothing, s')
            (Just x, s') -> runDefinite s' (f x)



getT :: Tentative s s
getT = Ten $ \s -> Just (s, s)

putT :: s -> Tentative s ()
putT s = Ten $ \_ -> Just ((), s)

throwT :: Tentative s a
throwT = Ten $ \_ -> Nothing

catchT :: Tentative s a -> Tentative s a -> Tentative s a
catchT (Ten tx) (Ten ty) = Ten $ \s ->
    case tx s of
        Nothing -> ty s
        Just (x, s') -> Just (x, s')



getD :: Definite s s
getD = Def $ \s -> (Just s, s)

putD :: s -> Definite s ()
putD s = Def $ \_ -> (Just (), s)

throwD :: Definite s ()
throwD = Def $ \s -> (Nothing, s)

catchD :: Definite s a -> Definite s a -> Definite s a
catchD (Def tx) (Def ty) = Def $ \s ->
    case tx s of
        (Nothing, s') -> ty s'
        (Just x, s') -> (Just x, s')



data Labyrinth = Exit | DeadEnd | Choice Labyrinth Labyrinth
    deriving Show

data Step = First | Second | Back
    deriving Show

ex1 :: Labyrinth
ex1 = (DeadEnd `Choice` DeadEnd) `Choice` (Exit `Choice` DeadEnd)



addStepD :: Step -> Definite [Step] ()
addStepD step = do
    steps <- getD
    putD $ steps ++ [step]

searchD :: Labyrinth -> Definite [Step] ()
searchD Exit = pure ()
searchD DeadEnd = throwD
searchD (Choice first second) =
    (addStepD First >> searchD first) `catchD` (addStepD Second >> searchD second)



addStepT :: Step -> Tentative [Step] ()
addStepT step = do
    steps <- getT
    putT $ steps ++ [step]

searchT :: Labyrinth -> Tentative [Step] ()
searchT Exit = pure ()
searchT DeadEnd = throwT
searchT (Choice first second) =
    (addStepT First >> searchT first) `catchT` (addStepT Second >> searchT second)
