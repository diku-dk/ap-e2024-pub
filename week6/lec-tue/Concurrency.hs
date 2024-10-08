import Control.Concurrent
  ( Chan,
    ThreadId,
    forkIO,
    newChan,
    readChan,
    threadDelay,
    writeChan,
  )
import Data.IORef

threadEx :: IO ()
threadEx = do
  v <- newIORef "foo"
  forkIO $ do
    writeIORef v "bar"
  s <- readIORef v
  putStrLn s

chanEx :: IO ()
chanEx = do
  c <- newChan
  forkIO $ do
    writeChan c 1
    writeChan c 2
    writeChan c 3
  x <- readChan c
  print x
  y <- readChan c
  print y
  z <- readChan c
  print z

data IncMsg = MsgInc (Chan Int)

incServer :: Chan IncMsg -> Int -> IO ()
incServer c counter = do
  msg <- readChan c
  case msg of
    MsgInc from -> do
      writeChan from counter
      incServer c (counter + 1)

sendInc :: Chan IncMsg -> IO Int
sendInc c = do
  from <- newChan
  writeChan c $ MsgInc from
  readChan from

useIncServer :: IO ()
useIncServer = do
  c <- newChan
  forkIO $ incServer c 0

  x <- sendInc c
  print x
  y <- sendInc c
  print y
  z <- sendInc c
  print z
