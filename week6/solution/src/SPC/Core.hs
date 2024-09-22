module SPC.Core
  ( -- * SPC startup
    SPC,
    startSPC,

    -- * Job functions
    Job (..),
    JobId,
    JobStatus (..),
    JobDoneReason (..),
    jobAdd,
    jobStatus,
    jobWait,
    jobCancel,
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

-- | A job that is to be enqueued in the glorious SPC.
data Job = Job
  { -- | The IO action that comprises the actual action of the job.
    jobAction :: IO (),
    -- | The maximum allowed runtime of the job, counting from when
    -- the job begins executing (not when it is enqueued).
    jobMaxSeconds :: Int
  }

-- | A unique identifier of a job that has been enqueued.
newtype JobId = JobId Int
  deriving (Eq, Ord, Show)

-- | How a job finished.
data JobDoneReason
  = -- | Normal termination.
    Done
  | -- | The job was killed because it ran for too long.
    DoneTimeout
  | -- | The job was explicitly cancelled.
    DoneCancelled
  | -- | The job crashed due to an exception.
    DoneCrashed
  deriving (Eq, Ord, Show)

-- | The status of a job.
data JobStatus
  = -- | The job is done and this is why.
    JobDone JobDoneReason
  | -- | The job is still running.
    JobRunning
  | -- | The job is enqueued, but is waiting for an idle worker.
    JobPending
  | -- | A job with this ID is not known to this SPC instance.
    JobUnknown
  deriving (Eq, Ord, Show)

-- Messages sent to SPC.
data SPCMsg
  = -- | Add the job, and reply with the job ID.
    MsgJobAdd Job (Chan JobId)
  | -- | Cancel the given job.
    MsgJobCancel JobId
  | -- | Immediately reply the status of the job.
    MsgJobStatus JobId (Chan JobStatus)
  | -- | Reply when the job is done.
    MsgJobWait JobId (Chan JobDoneReason)
  | -- | Job has finished.
    MsgJobDone JobId
  | -- | Job crashed.
    MsgJobCrashed JobId
  | -- | Some time has passed.
    MsgTick

-- | A Handle to the SPC instance.
data SPC = SPC (Chan SPCMsg)

-- | The central state. Must be protected from the bourgeoisie.
data SPCState = SPCState
  { spcChan :: Chan SPCMsg,
    spcJobsPending :: [(JobId, Job)],
    spcJobRunning :: Maybe (JobId, Seconds, ThreadId),
    spcJobsDone :: [(JobId, JobDoneReason)],
    -- | These are waiting for this job to terminate.
    spcWaiting :: [(JobId, Chan JobDoneReason)],
    spcJobCounter :: JobId
  }

-- | The monad in which the main SPC thread runs. This is a state
-- monad with support for IO.
newtype SPCM a = SPCM (SPCState -> IO (a, SPCState))

instance Functor SPCM where
  fmap = liftM

instance Applicative SPCM where
  pure x = SPCM $ \state -> pure (x, state)
  (<*>) = ap

instance Monad SPCM where
  SPCM m >>= f = SPCM $ \state -> do
    (x, state') <- m state
    let SPCM f' = f x
    f' state'

-- | Retrieve the state.
get :: SPCM SPCState
get = SPCM $ \state -> pure (state, state)

-- | Overwrite the state.
put :: SPCState -> SPCM ()
put state = SPCM $ \_ -> pure ((), state)

-- | Lift an 'IO' action into 'SPCM'.
io :: IO a -> SPCM a
io m = SPCM $ \state -> do
  x <- m
  pure (x, state)

-- | Run the SPCM monad.
runSPCM :: SPCState -> SPCM a -> IO a
runSPCM state (SPCM f) = fst <$> f state

schedule :: SPCM ()
schedule = do
  state <- get
  case (spcJobRunning state, spcJobsPending state) of
    (Nothing, (jobid, job) : jobs) -> do
      t <- io $ forkIO $ do
        let doJob = do
              jobAction job
              writeChan (spcChan state) $ MsgJobDone jobid
            onException :: SomeException -> IO ()
            onException _ =
              writeChan (spcChan state) $ MsgJobCrashed jobid
        doJob `catch` onException
      now <- io $ getSeconds
      let deadline = now + fromIntegral (jobMaxSeconds job)
      put $
        state
          { spcJobRunning = Just (jobid, deadline, t),
            spcJobsPending = jobs
          }
    _ -> pure ()

jobDone :: JobId -> JobDoneReason -> SPCM ()
jobDone jobid reason = do
  state <- get
  case lookup jobid $ spcJobsDone state of
    Just _ ->
      -- We already know this job is done.
      pure ()
    Nothing -> do
      let (waiting_for_job, not_waiting_for_job) =
            partition ((== jobid) . fst) (spcWaiting state)
      forM_ waiting_for_job $ \(_, c) ->
        io $ writeChan c reason
      put $
        state
          { spcWaiting = not_waiting_for_job,
            spcJobsDone = (jobid, reason) : spcJobsDone state,
            spcJobsPending = removeAssoc jobid $ spcJobsPending state
          }

checkTimeouts :: SPCM ()
checkTimeouts = do
  state <- get
  now <- io getSeconds
  case spcJobRunning state of
    Just (jobid, deadline, tid)
      | now >= deadline -> do
          io $ killThread tid
          put $ state {spcJobRunning = Nothing}
          jobDone jobid DoneTimeout
    _ -> pure ()

handleMsg :: Chan SPCMsg -> SPCM ()
handleMsg c = do
  checkTimeouts
  schedule
  msg <- io $ readChan c
  case msg of
    MsgJobAdd job reply -> do
      state <- get
      let JobId jobid = spcJobCounter state
      put $
        state
          { spcJobsPending =
              (spcJobCounter state, job) : spcJobsPending state,
            spcJobCounter = JobId $ succ jobid
          }
      io $ writeChan reply $ JobId jobid
    MsgJobStatus jobid reply -> do
      state <- get
      io $ writeChan reply $ case ( lookup jobid $ spcJobsPending state,
                                    spcJobRunning state,
                                    lookup jobid $ spcJobsDone state
                                  ) of
        (Just _, _, _) -> JobPending
        (_, Just (running_job, _, _), _)
          | running_job == jobid ->
              JobRunning
        (_, _, Just r) -> JobDone r
        _ -> JobUnknown
    MsgJobWait jobid reply -> do
      state <- get
      case lookup jobid $ spcJobsDone state of
        Just reason -> do
          io $ writeChan reply reason
        Nothing ->
          put $ state {spcWaiting = (jobid, reply) : spcWaiting state}
    MsgJobDone done_jobid -> do
      state <- get
      case spcJobRunning state of
        Just (jobid, _, _)
          | jobid == done_jobid ->
              jobDone jobid Done
        _ -> pure ()
    MsgJobCancel cancel_jobid -> do
      state <- get
      case spcJobRunning state of
        Just (jobid, _, tid) | jobid == cancel_jobid -> do
          io $ killThread tid
          jobDone jobid DoneCancelled
        _ -> pure ()
    MsgJobCrashed crashed_jobid -> do
      state <- get
      case spcJobRunning state of
        Just (jobid, _, tid) | jobid == crashed_jobid -> do
          io $ killThread tid
          jobDone jobid DoneCrashed
        _ -> pure ()
    MsgTick ->
      pure ()

startSPC :: IO SPC
startSPC = do
  c <- newChan
  let initial_state =
        SPCState
          { spcJobCounter = JobId 0,
            spcJobsPending = [],
            spcJobRunning = Nothing,
            spcJobsDone = [],
            spcWaiting = [],
            spcChan = c
          }
  void $ forkIO $ runSPCM initial_state $ forever $ handleMsg c
  void $ forkIO $ forever $ timer c
  pure $ SPC c
  where
    timer c = do
      threadDelay 1000000 -- 1 second
      writeChan c MsgTick

-- | Add a job for scheduling.
jobAdd :: SPC -> Job -> IO JobId
jobAdd (SPC c) job = do
  reply_chan <- newChan
  writeChan c $ MsgJobAdd job reply_chan
  readChan reply_chan

-- | Query the job status.
jobStatus :: SPC -> JobId -> IO JobStatus
jobStatus (SPC c) jobid = do
  reply_chan <- newChan
  writeChan c $ MsgJobStatus jobid reply_chan
  readChan reply_chan

-- | Synchronously block until job is done and return the reason.
jobWait :: SPC -> JobId -> IO JobDoneReason
jobWait (SPC c) jobid = do
  reply_chan <- newChan
  writeChan c $ MsgJobWait jobid reply_chan
  readChan reply_chan

-- | Asynchronously cancel a job.
jobCancel :: SPC -> JobId -> IO ()
jobCancel (SPC c) jobid =
  writeChan c $ MsgJobCancel jobid
