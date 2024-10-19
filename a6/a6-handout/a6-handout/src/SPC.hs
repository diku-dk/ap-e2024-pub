module SPC
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

    -- * Worker functions
    WorkerName,
    workerAdd,
    workerStop,
  )
where

import Control.Concurrent
  ( ThreadId,
    forkIO,
    killThread,
    newChan,
    threadDelay,
  )
import Control.Exception (SomeException, catch)
import Control.Monad (ap, forM_, forever, liftM, void)
import Data.List (find, partition)
import GenServer
import System.Clock.Seconds (Clock (Monotonic), Seconds, getTime)

-- First some general utility functions.

getSeconds :: IO Seconds
getSeconds = getTime Monotonic

removeAssoc :: (Eq k) => k -> [(k, v)] -> [(k, v)]
removeAssoc needle ((k, v) : kvs) =
  if k == needle
    then kvs
    else (k, v) : removeAssoc needle kvs
removeAssoc _ [] = []

-- Then the definition of the glorious SPC.

data Job = Job
  { jobAction :: IO (),
    jobMaxSeconds :: Int
  }

newtype JobId = JobId Int
  deriving (Eq, Ord, Show)

data JobDoneReason
  = Done
  | DoneTimeout
  | DoneCancelled
  | DoneCrashed
  deriving (Eq, Ord, Show)

data JobStatus
  = JobDone JobDoneReason
  | JobRunning
  | JobPending
  | JobUnknown
  deriving (Eq, Ord, Show)

type WorkerName = String

data WorkerMsg = WorkerJob JobId Job | WorkerStop

data SPCMsg
  = MsgJobAdd Job (ReplyChan JobId)
  | MsgJobCancel JobId
  | MsgJobStatus JobId (ReplyChan JobStatus)
  | MsgJobWait JobId (ReplyChan (Maybe JobDoneReason))
  | MsgTick
  | MsgWorkerAdd WorkerName (ReplyChan (Either String Worker))
  | MsgWorkerDone WorkerName JobId
  | MsgWorkerCrashed WorkerName JobId

data SPC = SPC (Server SPCMsg)

data Worker = Worker WorkerName ThreadId

data SPCState = SPCState
  { spcJobsPending :: [(JobId, Job)],
    spcJobsRunning :: [(JobId, Seconds, ThreadId)],
    spcJobsDone :: [(JobId, JobDoneReason)],
    spcJobCounter :: JobId,
    spcWorkers :: [(WorkerName, ThreadId, Chan WorkerMsg, Maybe JobId)],
    spcWaiting :: [(JobId, ReplyChan (Maybe JobDoneReason))],
    spcChan :: Chan SPCMsg
  }

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

get :: SPCM SPCState
get = SPCM $ \state -> pure (state, state)

put :: SPCState -> SPCM ()
put state = SPCM $ \_ -> pure ((), state)

modify :: (SPCState -> SPCState) -> SPCM ()
modify f = do
  state <- get
  put $ f state

io :: IO a -> SPCM a
io m = SPCM $ \state -> do
  x <- m
  pure (x, state)

runSPCM :: SPCState -> SPCM a -> IO a
runSPCM state (SPCM f) = fst <$> f state

schedule :: SPCM ()
schedule = do
  state <- get
  let idleWorkers = [(name, tid, chan) | (name, tid, chan, Nothing) <- spcWorkers state]
      pendingJobs = spcJobsPending state
  case (idleWorkers, pendingJobs) of
    ((workerName, workerTid, workerChan) : _, (jobId, job) : _) -> do
      now <- io getSeconds
      let deadline = now + fromIntegral (jobMaxSeconds job)
      let workers' = map (\(name, tid, chan, mJobId') ->
                            if name == workerName
                            then (name, tid, chan, Just jobId)
                            else (name, tid, chan, mJobId'))
                          (spcWorkers state)
          pendingJobs' = tail pendingJobs
          runningJobs' = (jobId, deadline, workerTid) : spcJobsRunning state
      put $ state
        { spcWorkers = workers',
          spcJobsPending = pendingJobs',
          spcJobsRunning = runningJobs'
        }
      io $ send workerChan $ WorkerJob jobId job
      schedule
    _ -> return ()

jobDone :: JobId -> JobDoneReason -> SPCM ()
jobDone jobId reason = do
  state <- get
  let runningJobs' = filter (\(jid, _, _) -> jid /= jobId) (spcJobsRunning state)
      (waiting_for_job, not_waiting_for_job) = partition ((== jobId) . fst) (spcWaiting state)
  forM_ waiting_for_job $ \(_, rsvp) ->
    io $ reply rsvp $ Just reason
  put $ state
    { spcJobsRunning = runningJobs',
      spcWaiting = not_waiting_for_job,
      spcJobsDone = (jobId, reason) : spcJobsDone state
    }

updateWorkerStatus :: WorkerName -> Maybe JobId -> SPCM ()
updateWorkerStatus workerName mJobId = do
  state <- get
  let workers' = map (\(name, tid, chan, oldJobId) ->
                        if name == workerName
                        then (name, tid, chan, mJobId)
                        else (name, tid, chan, oldJobId))
                      (spcWorkers state)
  put $ state { spcWorkers = workers' }

findWorkerByJobId :: JobId -> [(WorkerName, ThreadId, Chan WorkerMsg, Maybe JobId)] -> Maybe WorkerName
findWorkerByJobId jobId workers =
  case [name | (name, _, _, Just jId) <- workers, jId == jobId] of
    (name:_) -> Just name
    _ -> Nothing

checkTimeouts :: SPCM ()
checkTimeouts = do
  state <- get
  now <- io getSeconds
  let (timedOutJobs, runningJobs') = partition (\(_, deadline, _) -> now >= deadline) (spcJobsRunning state)
  forM_ timedOutJobs $ \(jobId, _, _) -> do
    jobDone jobId DoneTimeout
    let workerName = findWorkerByJobId jobId (spcWorkers state)
    case workerName of
      Just name -> updateWorkerStatus name Nothing
      Nothing -> return ()
  put $ state { spcJobsRunning = runningJobs' }

jobExists :: JobId -> SPCState -> Bool
jobExists jobId state =
  any ((== jobId) . fst) (spcJobsPending state) ||
  any (\(jid, _, _) -> jid == jobId) (spcJobsRunning state)

handleMsg :: Chan SPCMsg -> SPCM ()
handleMsg c = do
  checkTimeouts
  schedule
  msg <- io $ receive c
  case msg of
    MsgJobAdd job rsvp -> do
      state <- get
      let JobId jobid = spcJobCounter state
          newJobId = JobId $ jobid + 1
      put $
        state
          { spcJobsPending =
              (newJobId, job) : spcJobsPending state,
            spcJobCounter = newJobId
          }
      io $ reply rsvp newJobId
      schedule
    MsgJobStatus jobId rsvp -> do
      state <- get
      io $ reply rsvp $ case ( lookup jobId $ spcJobsPending state,
                               lookup jobId $ map (\(jid, _, _) -> (jid, ())) (spcJobsRunning state),
                               lookup jobId $ spcJobsDone state
                             ) of
        (Just _, _, _) -> JobPending
        (_, Just _, _) -> JobRunning
        (_, _, Just r) -> JobDone r
        _ -> JobUnknown
    MsgJobWait jobId rsvp -> do
      state <- get
      case lookup jobId $ spcJobsDone state of
        Just reason -> io $ reply rsvp $ Just reason
        Nothing ->
          if jobExists jobId state
            then put $ state { spcWaiting = (jobId, rsvp) : spcWaiting state }
            else io $ reply rsvp Nothing
    MsgJobCancel jobId -> do
      state <- get
      case lookup jobId $ map (\(jid, _, _) -> (jid, ())) (spcJobsRunning state) of
        Just _ -> do
          jobDone jobId DoneCancelled
          let workerName = findWorkerByJobId jobId (spcWorkers state)
          case workerName of
            Just name -> updateWorkerStatus name Nothing
            Nothing -> return ()
          schedule
        Nothing -> do
          let pendingJobs' = removeAssoc jobId (spcJobsPending state)
          put $ state { spcJobsPending = pendingJobs' }
          jobDone jobId DoneCancelled
    MsgWorkerAdd workerName rsvp -> do
      state <- get
      if any (\(name, _, _, _) -> name == workerName) (spcWorkers state)
        then io $ reply rsvp $ Left "Worker with this name already exists"
        else do
          workerChan <- io newChan
          workerTid <- io $ forkIO $ workerMain workerName workerChan (spcChan state)
          let worker = Worker workerName workerTid
          put $ state { spcWorkers = (workerName, workerTid, workerChan, Nothing) : spcWorkers state }
          io $ reply rsvp $ Right worker
          schedule
    MsgWorkerDone workerName jobId -> do
      jobDone jobId Done
      updateWorkerStatus workerName Nothing
      schedule
    MsgWorkerCrashed workerName jobId -> do
      jobDone jobId DoneCrashed
      updateWorkerStatus workerName Nothing
      schedule
    MsgTick -> return ()

startSPC :: IO SPC
startSPC = do
  c <- newChan
  let initial_state =
        SPCState
          { spcJobCounter = JobId 0,
            spcJobsPending = [],
            spcJobsRunning = [],
            spcJobsDone = [],
            spcWaiting = [],
            spcWorkers = [],
            spcChan = c
          }
  server <- spawn $ \_ -> runSPCM initial_state $ forever $ handleMsg c
  void $ spawn $ timer c
  pure $ SPC server
  where
    timer c _ = forever $ do
      threadDelay 1000000 -- 1 second
      send c MsgTick

jobAdd :: SPC -> Job -> IO JobId
jobAdd (SPC c) job =
  requestReply c $ MsgJobAdd job

jobStatus :: SPC -> JobId -> IO JobStatus
jobStatus (SPC c) jobid =
  requestReply c $ MsgJobStatus jobid

jobWait :: SPC -> JobId -> IO (Maybe JobDoneReason)
jobWait (SPC c) jobid =
  requestReply c $ MsgJobWait jobid

jobCancel :: SPC -> JobId -> IO ()
jobCancel (SPC c) jobid =
  sendTo c $ MsgJobCancel jobid

workerAdd :: SPC -> WorkerName -> IO (Either String Worker)
workerAdd (SPC c) workerName = requestReply c $ \rsvp -> MsgWorkerAdd workerName rsvp

workerStop :: Worker -> IO ()
workerStop = undefined

workerMain :: WorkerName -> Chan WorkerMsg -> Chan SPCMsg -> IO ()
workerMain workerName workerChan spcChan = do
  forever $ do
    msg <- receive workerChan
    case msg of
      WorkerJob jobId job -> do
        let doJob = do
              jobAction job
              send spcChan $ MsgWorkerDone workerName jobId
            onException :: SomeException -> IO ()
            onException _ =
              send spcChan $ MsgWorkerCrashed workerName jobId
        doJob `catch` onException
      WorkerStop -> return () -- Exits the worker thread
