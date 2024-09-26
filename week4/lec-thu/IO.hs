import Control.Exception
  ( Exception,
    SomeException,
    catch,
    evaluate,
  )

main :: IO ()
main = do
  putStrLn "hello"

dividesByZero :: Int -> Int
dividesByZero x = x `div` 0

tryDivide :: Int -> Int -> IO ()
tryDivide x y = do
  let whatCanFail :: IO ()
      whatCanFail = do
        print $ x `div` y
      handler :: SomeException -> IO ()
      handler e = do
        putStrLn $ show e
        putStrLn "it failed"
  catch whatCanFail handler

tryDivide2 :: Int -> Int -> IO Int
tryDivide2 x y = do
  let whatCanFail :: IO Int
      whatCanFail = do
        evaluate $ x `div` y
      handler :: SomeException -> IO Int
      handler e = do
        print e
        pure 0
  catch whatCanFail handler

--   ?

--   |
--   v

-- ? : ?

--   |
--   v

-- ? : []
