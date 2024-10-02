import Control.Monad (ap, forM, liftM, mapM)

forever :: (Monad m) => m a -> m b
forever m = do
  m
  forever m

data Free f a
  = Pure a
  | Free (f (Free f a))

instance (Functor f) => Functor (Free f) where
  fmap = liftM

instance (Functor f) => Applicative (Free f) where
  (<*>) = ap
  pure x = Pure x

instance (Functor f) => Monad (Free f) where
  Free x >>= y = Free $ h <$> x
    where
      h x' = x' >>= y
  Pure x >>= y = y x

type EventName = String

type EventValue = Int

type Event = (EventName, EventValue)

data EventOp a
  = WaitFor EventName (EventValue -> a)
  | Log String a

instance Functor EventOp where
  fmap f (WaitFor name c) =
    WaitFor name $ \v -> f (c v)
  fmap f (Log s c) = Log s (f c)

type EventM a = Free EventOp a

waitFor :: EventName -> EventM EventValue
waitFor name =
  Free $ WaitFor name $ \v -> pure v

logMsg :: String -> EventM ()
logMsg s =
  Free $ Log s $ pure ()

adder :: EventM ()
adder = do
  logMsg "adder starting"
  forever $ do
    x <- waitFor "add"
    y <- waitFor "add"
    logMsg $
      unwords
        [ show x,
          "+",
          show y,
          "=",
          show $ x + y
        ]

subber :: EventM ()
subber = do
  logMsg "subber starting"
  forever $ do
    x <- waitFor "sub"
    y <- waitFor "sub"
    logMsg $
      unwords
        [ show x,
          "-",
          show y,
          "=",
          show $ x - y
        ]

runUntilEvent :: EventM a -> IO (EventM a)
runUntilEvent (Pure x) = pure $ Pure x
runUntilEvent (Free (Log s c)) = do
  putStrLn s
  runUntilEvent c
runUntilEvent (Free (WaitFor name c)) =
  pure (Free (WaitFor name c))

runWithEvent ::
  EventM a ->
  Event ->
  IO (EventM a)
runWithEvent (Pure x) event = pure $ Pure x
runWithEvent (Free (Log s c)) event = do
  putStrLn s
  runWithEvent c event
runWithEvent
  (Free (WaitFor waiting_for c))
  (event_name, event_value) =
    if event_name == waiting_for
      then runUntilEvent $ c event_value
      else pure (Free (WaitFor waiting_for c))

runManyWithEvent ::
  [EventM ()] ->
  Event ->
  IO [EventM ()]
runManyWithEvent ps event =
  forM ps $ \p -> runWithEvent p event

main :: IO ()
main = do
  let ps = [adder, subber]
  ps <- mapM runUntilEvent ps
  ps <- runManyWithEvent ps ("add", 1)
  ps <- runManyWithEvent ps ("sub", 1)
  ps <- runManyWithEvent ps ("add", 3)
  ps <- runManyWithEvent ps ("sub", 4)
  pure ()

runEvents :: [EventM ()] -> IO ()
runEvents ps = do
  event <- readLn
  ps' <- runManyWithEvent ps event
  runEvents ps'
