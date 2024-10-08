import Control.Concurrent
  ( forkIO,
    killThread,
    threadDelay,
  )
import Control.Exception
  ( SomeException,
    catch,
    evaluate,
  )
import Control.Monad (forM_)
import GenServer

data Status a
  = Finished a
  | Error String
  | Timeout
  deriving (Show)

type Seconds = Int

poll :: Async a -> IO (Maybe (Status a))
poll (Async server) =
  requestReply server $ MsgPoll

wait :: Async a -> IO (Status a)
wait (Async server) =
  requestReply server $ MsgWait

data Async a = Async (Server (Msg a))

data Msg a
  = MsgPoll (ReplyChan (Maybe (Status a)))
  | MsgWait (ReplyChan (Status a))
  | MsgValueFinished a
  | MsgValueCrashed String
  | MsgTimeout

noValueLoop ::
  Chan (Msg a) ->
  [ReplyChan (Status a)] ->
  IO ()
noValueLoop c waiters = do
  msg <- receive c
  case msg of
    MsgPoll from -> do
      reply from Nothing
      noValueLoop c waiters
    MsgWait from -> do
      noValueLoop c (from : waiters)
    MsgValueCrashed e -> do
      forM_ waiters $ \waiter ->
        reply waiter $ Error e
      valueLoop c $ Error e
    MsgValueFinished v -> do
      forM_ waiters $ \waiter ->
        reply waiter $ Finished v
      valueLoop c $ Finished v
    MsgTimeout -> do
      forM_ waiters $ \waiter ->
        reply waiter Timeout
      valueLoop c Timeout

valueLoop :: Chan (Msg a) -> Status a -> IO ()
valueLoop c v = do
  msg <- receive c
  case msg of
    MsgPoll from -> do
      reply from $ Just v
      valueLoop c v
    MsgWait from -> do
      reply from v
      valueLoop c v
    MsgValueCrashed _ ->
      valueLoop c v
    MsgValueFinished _ ->
      valueLoop c v
    MsgTimeout ->
      valueLoop c v

async :: Seconds -> (a -> b) -> a -> IO (Async b)
async seconds f x = do
  server <- spawn $ \c -> do
    worker_tid <- forkIO $ do
      let onError :: SomeException -> IO ()
          onError e =
            send c $ MsgValueCrashed $ show e
          computation :: IO ()
          computation = do
            v <- evaluate $ f x
            send c $ MsgValueFinished v
      catch computation onError
    _ <- forkIO $ do
      threadDelay (seconds * 1000000)
      killThread worker_tid
      send c MsgTimeout
    noValueLoop c []
  pure $ Async server

fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib n =
  if n < 0
    then error "negative n"
    else fib (n - 2) + fib (n - 1)

testAsync :: IO ()
testAsync = do
  a <- async 2 fib 1000
  x <- poll a
  print x
  y <- wait a
  print y
  z <- wait a
  print z
