module APL.Parser (parseAPL) where

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
    satisfy,
    some,
    try,
  )
import Text.Megaparsec.Char (space)

type Parser = Parsec Void String

lexeme :: Parser a -> Parser a
lexeme p = p <* space

keywords :: [String]
keywords =
  [ "if",
    "then",
    "else",
    "true",
    "false",
    "let",
    "in",
    "try",
    "catch",
    "print",
    "put",
    "get"
  ]

lVName :: Parser VName
lVName = lexeme $ try $ do
  c <- satisfy isAlpha
  cs <- many $ satisfy isAlphaNum
  let v = c : cs
  if v `elem` keywords
    then fail "Unexpected keyword"
    else pure v

lInteger :: Parser Integer
lInteger =
  lexeme $ read <$> some (satisfy isDigit) <* notFollowedBy (satisfy isAlphaNum)

lString :: String -> Parser ()
lString s = lexeme $ void $ chunk s

lPrint :: Parser String
lPrint = lexeme $ try $ do
  lString "\""
  s <- many $ satisfy (/= '"')
  lString "\""
  pure s

lKeyword :: String -> Parser ()
lKeyword s = lexeme $ void $ try $ chunk s <* notFollowedBy (satisfy isAlphaNum)

lBool :: Parser Bool
lBool =
  lexeme . try . choice $
    [ const True <$> lKeyword "true",
      const False <$> lKeyword "false"
    ]

pAtom :: Parser Exp
pAtom =
  choice
    [ CstInt <$> lInteger,
      CstBool <$> lBool,
      Var <$> lVName,
      lString "(" *> pExp <* lString ")"
    ]

pFExp :: Parser Exp
pFExp =
  choice
    [ do
        func <- pAtom
        args <- many pAtom
        pure $ foldl Apply func args,
      pAtom
    ]

pLExp :: Parser Exp
pLExp =
  choice
    [ If
        <$> (lKeyword "if" *> pExp)
        <*> (lKeyword "then" *> pExp)
        <*> (lKeyword "else" *> pExp),
      Print <$> (lKeyword "print" *> lPrint) <*> pAtom,
      KvGet <$> (lKeyword "get" *> pAtom),
      KvPut <$> (lKeyword "put" *> pAtom) <*> pAtom,
      Lambda
        <$> (satisfy (== '\\') *> lVName <* lKeyword "->")
        <*> pExp,
      TryCatch <$> (lKeyword "try" *> pExp) <*> (lKeyword "catch" *> pExp),
      Let
        <$> (lKeyword "let" *> lVName <* lKeyword "=")
        <*> pExp
        <*> (lKeyword "in" *> pExp),
      pFExp
    ]

pExp2 :: Parser Exp
pExp2 = do
  x <- pLExp
  chain x
  where
    chain x =
      choice
        [ do
            lString "**"
            Pow x <$> pExp2,
          pure x
        ]

pExp1 :: Parser Exp
pExp1 = do
  x <- pExp2
  chain x
  where
    chain x =
      choice
        [ do
            lString "*"
            y <- pExp2
            chain $ Mul x y,
          do
            lString "/"
            y <- pExp2
            chain $ Div x y,
          pure x
        ]

pExp0 :: Parser Exp
pExp0 = pExp1 >>= chain
  where
    chain x =
      choice
        [ do
            lString "+"
            y <- pExp1
            chain $ Add x y,
          do
            lString "-"
            y <- pExp1
            chain $ Sub x y,
          pure x
        ]

pExp3 :: Parser Exp
pExp3 = pExp0 >>= chain
  where
    chain x =
      choice
        [ do
            lString "=="
            y <- pExp0
            chain $ Eql x y,
          pure x
        ]

pExp :: Parser Exp
pExp = pExp3

parseAPL :: FilePath -> String -> Either String Exp
parseAPL fname s = case parse (space *> pExp <* eof) fname s of
  Left err -> Left $ errorBundlePretty err
  Right x -> Right x
