module GenServer
  ( Chan,
    Server,
    receive,
    send,
    sendTo,
    spawn,
    ReplyWith,
    requestReply,
    reply,
  )
where

import Control.Concurrent (Chan)
import qualified Control.Concurrent as CC

data Server msg = Server CC.ThreadId (Chan msg)

data ReplyWith a = ReplyWith (Chan a)

send :: Chan a -> a -> IO ()
send chan msg =
  CC.writeChan chan msg

sendTo :: Server a -> a -> IO ()
sendTo (Server _tid input) msg =
  send input msg

receive :: Chan a -> IO a
receive = CC.readChan

spawn :: (Chan a -> IO ()) -> IO (Server a)
spawn server = do
  input <- CC.newChan
  tid <- CC.forkIO $ server input
  pure $ Server tid input

requestReply :: Server a -> (ReplyWith b -> a) -> IO b
requestReply serv con = do
  reply_chan <- CC.newChan
  sendTo serv $ con $ ReplyWith reply_chan
  receive reply_chan

reply :: ReplyWith a -> a -> IO ()
reply (ReplyWith chan) x = send chan x
