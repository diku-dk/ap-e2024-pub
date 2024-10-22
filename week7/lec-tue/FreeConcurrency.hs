import Control.Concurrent
  ( Chan,
    forkIO,
    newChan,
    readChan,
    writeChan,
  )
import Control.Monad (ap, forever, liftM)

-- ANCHOR: State
newtype State s a = State (s -> (a, s))

-- ANCHOR_END: State

-- ANCHOR: Functor_State
instance Functor (State s) where
  fmap = liftM

-- ANCHOR_END: Functor_State

-- ANCHOR: Applicative_State
instance Applicative (State s) where
  pure x = State $ \state -> (x, state)
  (<*>) = ap

-- ANCHOR_END: Applicative_State

-- ANCHOR: Monad_State
instance Monad (State s) where
  State m >>= f = State $ \state ->
    let (x, state') = m state
        State f' = f x
     in f' state'

-- ANCHOR_END: Monad_State

runState :: s -> State s a -> (a, s)
runState s (State f) = f s

get :: State s s
get = State $ \s -> (s, s)

put :: s -> State s ()
put s = State $ \_ -> ((), s)

data Free e a
  = Pure a
  | Free (e (Free e a))

instance (Functor e) => Functor (Free e) where
  fmap f (Pure x) = Pure $ f x
  fmap f (Free g) = Free $ fmap (fmap f) g

instance (Functor e) => Applicative (Free e) where
  pure = Pure
  (<*>) = ap

instance (Functor e) => Monad (Free e) where
  Pure x >>= f = f x
  Free g >>= f = Free $ h <$> g
    where
      h x = x >>= f

type Msg = String

data CCOp chan a
  = CCNewChan (chan -> a)
  | CCReadChan chan (Msg -> a)
  | CCWriteChan chan Msg a
  | CCFork (CC chan ()) a

instance Functor (CCOp chan) where
  fmap f (CCNewChan c) =
    CCNewChan (f . c)
  fmap f (CCReadChan chan c) =
    CCReadChan chan (f . c)
  fmap f (CCWriteChan chan msg c) =
    CCWriteChan chan msg (f c)
  fmap f (CCFork t c) =
    CCFork t (f c)

type CC chan a = Free (CCOp chan) a

ccNewChan :: CC chan chan
ccNewChan =
  Free $ CCNewChan pure

ccReadChan :: chan -> CC chan Msg
ccReadChan chan =
  Free $ CCReadChan chan pure

ccWriteChan :: chan -> Msg -> CC chan ()
ccWriteChan chan msg =
  Free $ CCWriteChan chan msg $ pure ()

ccFork :: CC chan () -> CC chan ()
ccFork t =
  Free $ CCFork t $ pure ()

chain :: CC chan String
chain = do
  chan_0 <- ccNewChan
  chan_1 <- ccNewChan
  chan_2 <- ccNewChan
  chan_3 <- ccNewChan
  let link from to = do
        msg <- ccReadChan from
        ccWriteChan to $ msg <> "x"
  ccFork $ link chan_0 chan_1
  ccFork $ link chan_1 chan_2
  ccFork $ link chan_2 chan_3
  ccWriteChan chan_0 ""
  ccReadChan chan_3

interpCCIO :: CC (Chan Msg) a -> IO a
interpCCIO (Pure x) = pure x
interpCCIO (Free (CCNewChan c)) = do
  chan <- newChan
  interpCCIO $ c chan
interpCCIO (Free (CCReadChan chan c)) = do
  msg <- readChan chan
  interpCCIO $ c msg
interpCCIO (Free (CCWriteChan chan msg c)) = do
  writeChan chan msg
  interpCCIO c
interpCCIO (Free (CCFork t c)) = do
  _ <- forkIO $ interpCCIO t
  interpCCIO c

newtype ChanId = ChanId Int
  deriving (Eq)

data CCState = CCState
  { ccChannels :: [(ChanId, [Msg])],
    ccCounter :: ChanId,
    ccThreads :: [CC ChanId ()]
  }

initialState :: CCState
initialState =
  CCState
    { ccChannels = [],
      ccCounter = ChanId 0,
      ccThreads = []
    }

incCounter :: State CCState ChanId
incCounter = do
  state <- get
  let ChanId x = ccCounter state
  put $ state {ccCounter = ChanId (x + 1)}
  pure $ ChanId x

setChan ::
  ChanId ->
  [Msg] ->
  State CCState ()
setChan chan_id msgs = do
  state <- get
  put $
    state
      { ccChannels =
          (chan_id, msgs)
            : filter
              ((/= chan_id) . fst)
              (ccChannels state)
      }

getChan :: ChanId -> State CCState [Msg]
getChan chan_id = do
  state <- get
  case lookup chan_id $ ccChannels state of
    Just msgs -> pure msgs
    Nothing -> error "unknown channel"

addThread ::
  CC ChanId () ->
  State CCState ()
addThread t = do
  state <- get
  put $
    state
      { ccThreads =
          t : ccThreads state
      }

step ::
  CC ChanId a ->
  State CCState (CC ChanId a)
step (Pure x) = pure $ Pure x
step (Free (CCNewChan c)) = do
  chan_id <- incCounter
  setChan chan_id []
  pure $ c chan_id
step (Free (CCReadChan chan_id c)) = do
  msgs <- getChan chan_id
  case msgs of
    msg : msgs' -> do
      setChan chan_id msgs'
      pure $ c msg
    [] -> pure $ Free (CCReadChan chan_id c)
step (Free (CCWriteChan chan_id msg c)) = do
  msgs <- getChan chan_id
  setChan chan_id $ msgs ++ [msg]
  pure c
step (Free (CCFork t c)) = do
  addThread t
  pure c

stepThreads :: State CCState ()
stepThreads = do
  old_state <- get
  put $ old_state {ccThreads = []}
  threads <- mapM step $ ccThreads old_state
  new_state <- get
  put $
    new_state
      { ccThreads =
          threads ++ ccThreads new_state
      }

interp :: CC ChanId a -> State CCState a
interp (Pure x) = pure x
interp cc = do
  stepThreads
  cc' <- step cc
  interp cc'

interpCCPure :: CC ChanId a -> a
interpCCPure cc =
  fst $ runState initialState $ interp cc

infiniteWrite :: CC chan String
infiniteWrite = do
  chan <- ccNewChan
  ccFork $ forever $ ccWriteChan chan "x"
  a <- ccReadChan chan
  b <- ccReadChan chan
  pure $ a ++ b

infiniteThread :: CC chan String
infiniteThread = do
  chan <- ccNewChan
  ccFork $ forever $ pure ()
  ccFork $ ccWriteChan chan "x"
  ccReadChan chan
