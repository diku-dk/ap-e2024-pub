import Control.Applicative (Alternative (..))
import Control.Monad (ap, guard, liftM, void)
import Data.Char (isAlpha, isDigit, ord)

digitsToInt :: String -> Int
digitsToInt s = loop 1 $ reverse s
  where
    loop _ [] = 0
    loop w (c : cs) =
      (ord c - ord '0') * w
        + loop (w * 10) cs

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
  _ <- many $ satisfy (== ' ')
  pure ()

parseTwoInts :: Parser (Int, Int)
parseTwoInts = do
  x <- parseInt
  space
  y <- parseInt <?> "expected second integer"
  pure (x, y)

--
-- Exp ::= "true"
--       | "false"
--       | "not" Exp
--       | Exp "and" Exp
--       | Exp "or" Exp
--       | id
--       | ( Exp )
--
-- an 'id' consists of one or more alphabetic characters
--
-- true and (false and x)

data Exp
  = ExpTrue
  | ExpFalse
  | Id String
  | Not Exp
  | And Exp Exp
  | Or Exp Exp
  | Parens Exp
  deriving (Eq, Ord, Show)

string :: String -> Parser ()
string [] = pure ()
string (c : cs) = do
  _ <- satisfy (== c)
  string cs

notFollowedBy :: Parser a -> Parser ()
notFollowedBy (Parser p) = Parser $ \(counter, s) ->
  case p (counter, s) of
    Right _ -> Left (counter, "should not succeed")
    Left _ -> Right ((), (counter, s))

wordConstituent :: Char -> Bool
wordConstituent c = isAlpha c

lexeme :: Parser a -> Parser a
lexeme p = do
  x <- p
  space
  pure x

lKeyword :: String -> Parser ()
lKeyword s = lexeme $ do
  string s
  notFollowedBy $ satisfy wordConstituent

between ::
  Parser l ->
  Parser r ->
  Parser a ->
  Parser a
between l r p = do
  void l
  x <- p
  void r
  pure x

lLeftParen :: Parser ()
lLeftParen = lexeme $ void $ satisfy (== '(')

lRightParen :: Parser ()
lRightParen = lexeme $ void $ satisfy (== ')')

parens :: Parser a -> Parser a
parens = between lLeftParen lRightParen

keywords :: [String]
keywords = ["not", "true", "false", "and", "or"]

lIdentifier :: Parser String
lIdentifier = lexeme $ do
  s <- some $ satisfy wordConstituent
  guard $ not $ s `elem` keywords
  pure s

pAtom :: Parser Exp
pAtom =
  choice
    [ Id <$> lIdentifier,
      do
        lKeyword "true"
        pure ExpTrue,
      do
        lKeyword "false"
        pure ExpFalse,
      do
        lKeyword "not"
        e <- pExp
        pure $ Not e,
      do
        Parens <$> parens pExp
    ]

pExp1 :: Parser Exp
pExp1 = do
  x <- pAtom
  chain x
  where
    chain x =
      choice
        [ do
            lKeyword "and"
            y <- pAtom
            chain $ And x y,
          pure x
        ]

pExp0 :: Parser Exp
pExp0 = do
  x <- pExp1
  chain x
  where
    chain x =
      choice
        [ do
            lKeyword "or"
            y <- pExp1
            chain $ Or x y,
          pure x
        ]

pExp :: Parser Exp
pExp = pExp0

-- Atom ::= "true"
--        | "false"
--        | "not" Exp
--        | id
--        | ( Exp )
--
-- Exp1' ::= (* empty *)
--        | "and" Atom Exp1'
--
-- Exp1 ::= Atom Exp1'
--
-- Exp0' ::= (* empty *)
--        | "or" Exp1' Exp0'
--
-- Exp0 ::= Exp1' Exp0'
--
-- Exp ::= Exp0
--
-- x op y op .... op z

parseExp :: String -> Either Error Exp
parseExp input =
  case runParser input p of
    Left err -> Left err
    Right e -> Right e
  where
    p = do
      space
      x <- pExp
      eof
      pure x
