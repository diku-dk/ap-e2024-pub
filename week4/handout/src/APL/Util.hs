{-# LANGUAGE FlexibleInstances #-}

module APL.Util
  ( Serialize (..),
    newTempDB,
    captureIO,
  )
where

import APL.Monad
import Control.Concurrent (threadDelay)
import Control.Exception (bracket)
import Control.Monad (guard)
import Data.Char (isDigit)
import Data.List (stripPrefix)
import Data.Maybe (mapMaybe)
import GHC.IO.Handle (hDuplicate, hDuplicateTo)
import System.Directory (listDirectory)
import System.IO
import System.Process (createPipe)
import Text.Read (readMaybe)

class Serialize a where
  serialize :: a -> String
  unserialize :: String -> Maybe a

instance Serialize Val where
  serialize (ValInt x) = "ValInt " <> show x
  serialize (ValBool b) = "ValBool " <> show b
  serialize ValFun {} = error "ValFun serialization is not supported."

  unserialize s =
    case words s of
      ["ValInt", rest]
        | all isDigit rest -> ValInt <$> readMaybe rest
      ["ValBool", rest] -> ValBool <$> readMaybe rest
      _ -> Nothing

instance (Serialize a, Serialize b) => Serialize (a, b) where
  serialize (a, b) =
    serialize a ++ "," ++ serialize b
  unserialize s =
    case span (/= ',') s of
      (s_a, _ : s_b) -> (,) <$> unserialize s_a <*> unserialize s_b
      _ -> Nothing

instance Serialize [(Val, Val)] where
  serialize kv =
    unlines $ map serialize kv
  unserialize =
    mapM unserialize . lines

newTempDB :: IO FilePath
newTempDB = do
  files <- listDirectory "."
  let n = maximum (0 : mapMaybe match files) + 1
      tempFile = "temp" ++ show n ++ ".txt"
  writeFile tempFile ""
  pure $ tempFile
  where
    match :: FilePath -> Maybe Int
    match s = do
      s' <- stripPrefix "temp" s
      let (n_s, rest) = (takeWhile isDigit s', dropWhile isDigit s')
      guard $ rest == ".txt"
      readMaybe n_s

captureIO :: [String] -> IO a -> IO ([String], a)
captureIO inputs m = do
  hFlush stdout
  threadDelay 50000 -- Needed to make sure things are actually flushed
  stdin' <- hDuplicate stdin
  stdout' <- hDuplicate stdout

  (inR, inW) <- createPipe
  (outR, outW) <- createPipe

  hSetBuffering inW NoBuffering
  hSetBuffering outW NoBuffering

  bracket
    ( do
        inR `hDuplicateTo` stdin
        outW `hDuplicateTo` stdout
    )
    ( \_ -> do
        stdin' `hDuplicateTo` stdin
        stdout' `hDuplicateTo` stdout
        mapM_ hClose [stdin', stdout', inR, inW, outW]
    )
    ( \_ -> do
        mapM_ (hPutStrLn inW) inputs
        hFlush inW

        res <- m

        output <- hGetContents outR -- hGetContents closes outR
        pure (lines output, res)
    )
