module SPC.Core
  ( -- * SPC startup
    SPC,
    startSPC,
  )
where

import Control.Concurrent
  ( Chan,
    ThreadId,
    forkIO,
    killThread,
    newChan,
    readChan,
    threadDelay,
    writeChan,
  )
import Control.Exception (SomeException, catch)
import Control.Monad (ap, forM_, forever, liftM, void)
import Data.List (partition)
import System.Clock.Seconds (Clock (Monotonic), Seconds, getTime)

-- First some general utility functions.

-- | Retrieve Unix time using a monotonic clock. You cannot use this
-- to measure the actual world time, but you can use it to measure
-- elapsed time.
getSeconds :: IO Seconds
getSeconds = getTime Monotonic

-- | Remove mapping from association list.
removeAssoc :: (Eq k) => k -> [(k, v)] -> [(k, v)]
removeAssoc needle ((k, v) : kvs) =
  if k == needle
    then kvs
    else (k, v) : removeAssoc needle kvs
removeAssoc _ [] = []

-- Then the definition of the glorious SPC.

-- Messages sent to SPC.
data SPCMsg -- TODO: add messages.

-- | A Handle to the SPC instance.
data SPC = SPC (Chan SPCMsg)

-- | The central state. Must be protected from the bourgeoisie.
data SPCState = SPCState
  { spcChan :: Chan SPCMsg
  }

startSPC :: IO SPC
startSPC = do
  c <- newChan
  pure $ SPC c
