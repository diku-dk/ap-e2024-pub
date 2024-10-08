module GenServer
  ( Server,
    spawn,
    send,
    receive,
    sendTo,
    requestReply,
    Chan,
    reply,
    ReplyChan,
  )
where

import Control.Concurrent
  ( Chan,
    ThreadId,
    forkIO,
    newChan,
    readChan,
    writeChan,
  )

data Server msg = Server ThreadId (Chan msg)

spawn :: (Chan msg -> IO ()) -> IO (Server msg)
spawn f = do
  c <- newChan
  tid <- forkIO $ f c
  pure $ Server tid c

send :: Chan msg -> msg -> IO ()
send = writeChan

receive :: Chan msg -> IO msg
receive = readChan

sendTo :: Server msg -> msg -> IO ()
sendTo (Server _ c) x = writeChan c x

data ReplyChan a = ReplyChan (Chan a)

reply :: ReplyChan a -> a -> IO ()
reply (ReplyChan c) x = send c x

requestReply ::
  Server msg ->
  (ReplyChan a -> msg) ->
  IO a
requestReply server mkMsg = do
  from <- newChan
  let msg = mkMsg $ ReplyChan from
  sendTo server msg
  receive from
