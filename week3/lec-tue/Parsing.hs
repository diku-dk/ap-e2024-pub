import Control.Applicative (Alternative (..))
import Control.Monad (ap, guard, liftM)
import Data.Char (isDigit, ord)

digitsToInt :: String -> Int
digitsToInt s = loop 1 $ reverse s
  where
    loop _ [] = 0
    loop w (c : cs) =
      (ord c - ord '0') * w
        + loop (w * 10) cs

digitsToIntMaybe :: String -> Maybe Int
digitsToIntMaybe s =
  digitsToInt . reverse <$> check [] s
  where
    check acc (c : cs) =
      if isDigit c
        then check (c : acc) cs
        else Nothing
    check acc [] = Just acc

readTwoInts :: String -> Maybe (Int, Int)
readTwoInts s = loop1 [] s
  where
    loop1 acc (c : cs) =
      if isDigit c
        then loop1 (c : acc) cs
        else
          if c == ' '
            then do
              x <- loop2 [] cs
              pure
                ( digitsToInt $ reverse acc,
                  x
                )
            else Nothing
    loop1 _ [] =
      Nothing
    loop2 acc (c : cs) =
      if isDigit c
        then loop2 (c : acc) cs
        else Nothing
    loop2 acc [] =
      Just $ digitsToInt $ reverse acc

readInt :: String -> Maybe (Int, String)
readInt s = do
  (digits, residual) <- check [] s
  guard $ not $ null digits
  pure
    ( digitsToInt $ reverse digits,
      residual
    )
  where
    check acc (c : cs) =
      if isDigit c
        then check (c : acc) cs
        else Just (acc, (c : cs))
    check acc [] = Just (acc, [])

readTwoInts2 ::
  String ->
  Maybe ((Int, Int), String)
readTwoInts2 s =
  case readInt s of
    Nothing -> Nothing
    Just (x, s') ->
      case s' of
        ' ' : s'' ->
          case readInt s'' of
            Nothing -> Nothing
            Just (y, s''') ->
              Just ((x, y), s''')
        _ -> Nothing

readManyInts ::
  String ->
  Maybe ([Int], String)
readManyInts s =
  case readInt s of
    Nothing -> Nothing
    Just (x, s') ->
      case s' of
        ' ' : s'' ->
          case readManyInts s'' of
            Nothing -> Just ([x], s'')
            Just (xs, s''') ->
              Just (x : xs, s''')
        _ -> Just ([x], s')

type Error = String

data Parser a
  = Parser
      ( (Int, String) ->
        Either (Int, Error) (a, (Int, String))
      )

instance Functor Parser where
  fmap = liftM

instance Applicative Parser where
  pure x = Parser $ \s ->
    Right (x, s)
  (<*>) = ap

instance Monad Parser where
  Parser m >>= f =
    Parser $ \s ->
      case m s of
        Left err -> Left err
        Right (x, s') ->
          let Parser f' = f x
           in f' s'

instance MonadFail Parser where
  fail = parserFailure

instance Alternative Parser where
  empty = parserFailure "empty"
  Parser x <|> Parser y =
    Parser $ \s ->
      case x s of
        Right (x', s') ->
          Right (x', s')
        Left _ ->
          y s

next :: Parser Char
next = Parser $ \s ->
  case s of
    (counter, []) ->
      Left (counter, "end of input")
    (counter, c : s') ->
      Right (c, (counter + 1, s'))

parserFailure :: String -> Parser a
parserFailure err =
  Parser $ \(counter, _) -> Left (counter, err)

choice ::
  (Alternative f) =>
  [f a] ->
  f a
choice [] = empty
choice (p : ps) = p <|> choice ps

eof :: Parser ()
eof = Parser $ \s ->
  case s of
    (counter, []) ->
      Right ((), (counter, []))
    (counter, _) ->
      Left (counter, "expected end of input")

(<?>) :: Parser a -> String -> Parser a
Parser x <?> err = Parser $ \s ->
  case x s of
    Left (counter, _) -> Left (counter, err)
    Right x' -> Right x'

runParser ::
  String ->
  Parser a ->
  Either Error a
runParser s (Parser f) =
  case f (0, s) of
    Left (counter, err) ->
      Left $
        "Error at character offset "
          ++ show counter
          ++ ":\n"
          ++ err
    Right (x, _) -> Right x

parseDigit :: Parser Int
parseDigit = do
  c <- next
  if isDigit c
    then pure (ord c - ord '0')
    else parserFailure "expected digit"

parseTwoDigits :: Parser (Int, Int)
parseTwoDigits = do
  x <- parseDigit
  y <- parseDigit
  pure (x, y)

parseInt :: Parser Int
parseInt = do
  digits <-
    some parseDigit
      <?> "expected integer"
  pure $ loop 1 $ reverse digits
  where
    loop _ [] = 0
    loop w (c : cs) =
      c * w + loop (w * 10) cs

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = do
  c <- next
  if p c
    then pure c
    else parserFailure "invalid input"

space :: Parser ()
space = do
  ' ' <- next
  pure ()

spaces :: Parser ()
spaces = do
  _ <- many $ satisfy (== ' ')
  pure ()

parseTwoInts :: Parser (Int, Int)
parseTwoInts = do
  x <- parseInt
  spaces
  y <- parseInt <?> "expected second integer"
  pure (x, y)
