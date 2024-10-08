import GenServer

data IncMsg = MsgInc Int (ReplyChan Int)

incServer :: Chan IncMsg -> Int -> IO ()
incServer c counter = do
  msg <- receive c
  case msg of
    MsgInc delta from -> do
      reply from (counter + delta)
      incServer c (counter + delta)

sendInc :: Server IncMsg -> Int -> IO Int
sendInc server delta = do
  requestReply server $ MsgInc delta

useIncServer :: IO ()
useIncServer = do
  server <- spawn $ \c -> incServer c 0

  x <- sendInc server 1
  print x
  y <- sendInc server 2
  print y
  z <- sendInc server 3
  print z
