module APL.Parser (parseAPL, lInteger) where

import APL.AST (Exp (..), VName)
import Control.Monad (void)
import Data.Char (isAlpha, isAlphaNum, isDigit)
import Data.Void (Void)
import Text.Megaparsec
  ( Parsec,
    choice,
    chunk,
    eof,
    errorBundlePretty,
    many,
    notFollowedBy,
    parse,
    parseTest,
    satisfy,
    some,
    try,
  )
import Text.Megaparsec.Char (space)

-- Do not change this definition.
type Parser = Parsec Void String

pAtom :: Parser Exp
pAtom =
  choice
    [ CstInt <$> lInteger,
      CstBool <$> lBool,
      Var <$> lVName,
      lString "(" *> pExp <* lString ")"
    ]

pExp1 :: Parser Exp
pExp1 = pAtom >>= chain
  where
    chain x =
      choice
        [ do
            lString "*"
            y <- pAtom
            chain (Mul x y),
          do
            lString "/"
            y <- pAtom
            chain (Div x y),
          pure x
        ]

pExp :: Parser Exp
pExp = pExp0

lExp :: Parser Exp
lExp = do
  lKeyword "if"
  e1 <- pExp
  lKeyword "then"
  e2 <- pExp
  lKeyword "else"
  e3 <- pExp
  pure (If e1 e2 e3)

pExp0 :: Parser Exp
pExp0 = pExp1 >>= chain
  where
    chain x =
      choice
        [ do
            lString "+"
            y <- pExp1
            chain (Add x y),
          do
            lString "-"
            y <- pExp1
            chain (Sub x y),
          pure x
        ]

lInteger :: Parser Integer
-- Make sure that int is not followed by an alphanumeric character.
lInteger = lexeme $ read <$> some (satisfy isDigit) <* notFollowedBy (satisfy isAlphaNum)

keyWords :: [String]
keyWords = ["true", "false", "if", "then", "else", "let", "in", "try", "catch", "print", "put", "get"]

lVName :: Parser VName
lVName = lexeme $ try $ do
  c <- satisfy isAlpha
  cs <- many (satisfy isAlphaNum)
  if (c : cs) `elem` keyWords
    then
      fail "Unexpected keyword"
    else
      pure (c : cs)

lKeyword :: String -> Parser ()
lKeyword s = lexeme $ void $ try $ chunk s <* notFollowedBy (satisfy isAlphaNum)

lString :: String -> Parser ()
lString s = lexeme $ void $ chunk s

lBool :: Parser Bool
lBool = try $ lexeme $ choice [True <$ lKeyword "true", False <$ lKeyword "false"]

lexeme :: Parser a -> Parser a
lexeme p = p <* space

-- Do not change this definition.
parseAPL :: FilePath -> String -> Either String Exp
parseAPL fname s = case parse (space *> pAtom <* eof) fname s of
  Left err -> Left $ errorBundlePretty err
  Right x -> Right x
