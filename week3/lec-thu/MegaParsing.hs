import Control.Monad (guard, void)
import Data.Char (isAlpha)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char (space)

type Parser a = Parsec Void String a

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

lLeftParen :: Parser ()
lLeftParen = lexeme $ void $ satisfy (== '(')

lRightParen :: Parser ()
lRightParen = lexeme $ void $ satisfy (== ')')

parens :: Parser a -> Parser a
parens p = lLeftParen *> p <* lRightParen

keywords :: [String]
keywords = ["not", "true", "false", "and", "or"]

lIdentifier :: Parser String
lIdentifier = lexeme $ try $ do
  s <- some $ satisfy wordConstituent
  guard $ not $ s `elem` keywords
  pure s

pAtom :: Parser Exp
pAtom =
  choice
    [ Id <$> lIdentifier,
      pure ExpTrue <* lKeyword "true",
      pure ExpTrue <* lKeyword "false",
      Not <$> (lKeyword "not" *> pExp),
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

parseExp :: String -> Either String Exp
parseExp input =
  case runParser p "user input" input of
    Left err -> Left $ errorBundlePretty err
    Right e -> Right e
  where
    p = do
      space
      x <- pExp
      eof
      pure x
