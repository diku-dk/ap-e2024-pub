module Main (main) where

import APL.Eval (Val (..), eval, runEval)
import APL.Parser (parseAPL)
import System.Environment
  ( getArgs,
    getProgName,
  )
import System.Exit (ExitCode (..), exitWith)
import System.IO (hPutStrLn, stderr, stdout)

stringVal :: Val -> String
stringVal (ValBool b) = show b
stringVal (ValInt x) = show x
stringVal ValFun {} = "#<fun>"

main :: IO ()
main = do
  args <- getArgs
  case args of
    [fname] -> do
      s <- readFile fname
      case parseAPL fname s of
        Left err -> hPutStrLn stderr err
        Right e -> case runEval (eval e) of
          Left err -> hPutStrLn stderr $ show err
          Right v -> hPutStrLn stdout $ stringVal v
    _ -> do
      prog <- getProgName
      failure $ "Usage: " ++ prog ++ " FILE"
  pure ()
  where
    failure e = do
      hPutStrLn stderr $ show e
      exitWith $ ExitFailure 1
