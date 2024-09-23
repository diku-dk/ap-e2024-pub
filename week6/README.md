# Week 6 - Concurrency

## Slides and Material

TBA

## Suggested Reading

* [Concurrent Haskell](concurrent-haskell.pdf)

### Going Beyond

* [The three kinds of Haskell exceptions and how to use
  them](https://www.tweag.io/blog/2020-04-16-exceptions-in-haskell/)

## Exercises

In these exercises, and the associated assignment, you will be
implementing the [*Stateful Planning
Committee*](https://en.wikipedia.org/wiki/Gosplan) (SPC), a *job
scheduler* for managing the employment of workers and allocation of
resources. A job is any Haskell computation in the `IO` monad. A job
does not return any value, but is executed solely for to its side
effects. After a job is *enqueued* in SPC, we can ask for the status
of the job. At some point, SPC will decide to actually execute the
job.

We can imagine using a job scheduler such as SPC to enqueue thousands
of jobs for downloading various files from the Internet, with the
scheduler taking care of ensuring only a limited number of jobs run
concurrently, handling timeouts, performing logging, and so on. Our
scheduler will be fairly simple, but will have a lot of the machinery
you also need in a real scheduler.

SPC will take the form of various Haskell threads that communicate
through message passing. The main challenge is that the jobs are
*untrusted*, and may signal errors and go into infinite loops, which
must be handled without any damage to the system as a whole. In
particular, there will be a central *SPC thread* which we must protect
by ensuring evaluation of untrusted code happens in separate threads,
and with appropriate handlers.

There are some limits to the antagonism we will assume of enqueued
work: we cannot cope with work that uses functions from
[System.Exit](https://hackage.haskell.org/package/base-4.20.0.1/docs/System-Exit.html)
to shut down the entire process, or maliciously try to subvert SPC
itself. Since jobs are (for now) arbitrary `IO` actions, there is not
any true security in SPC.

This exercise permits you somewhat more freedom than most others. In
particular, you are expected to largely design the message types
yourself.

## Creating the Event Loop

You will find a skeletal code handout in [handout/](handout/). The
module `SPC.Core` contains two utility functions that will become
useful later (`getSeconds` and `removeAssoc`). It also contains two
types (`SPCMsg`, `SPC`) and one value definition (`startSPC`). It also
contains a bunch of imports that will become necessary, but adds a lot
of warning clutter - feel free to comment them until you need them.

The `SPC` type is our handle to the SPC, and it is complete - you do
not need to modify it.

The `SPCMsg` type represents messages that will be sent *to* the SPC
thread. We will extend this type with new messages as we go along.

The first thing you must do is to extend `startSPC` such that it
launches a new thread that runs a loop that reads messages from the
created channel and acts on them. To do this:

1. Add a constructor `MsgPing (Chan Int)` to `SPCMsg`. We will remove
   this later, but it is useful for testing that we get the event loop
   right.

2. Modify `startSPC` such that it creates a thread that runs in an
   infinite loop. This thread runs in an infinite loop and reads
   `SPCMsg` messages. When it receives a `MsgPing` message, it should
   send back an integer (your choice) on the channel in the `MsgPing`
   message.

3. Write a function `pingSPC :: SPC -> IO Int` that sends a `MsgPing`
   message to SPC, waits for a response, then returns the integer in
   the response. You must also add `pingSPC` to the module export
   list.

4. Add a test to `SPC.Core_Tests` that exercises `pingSPC`.

### Hints

Use `forever` from `Control.Monad` to write infinite monadic loops.

### Solution

<details>
<summary>Open this to see the implementation</summary>

```Haskell

data SPCMsg = MsgPing (Chan Int)

startSPC :: IO SPC
startSPC = do
  c <- newChan
  _ <- forkIO $ forever $ handle c
  pure $ SPC c
  where
    handle c = do
      msg <- readChan c
      case msg of
        MsgPing reply_chan ->
          writeChan reply_chan 1337

pingSPC :: SPC -> IO Int
pingSPC (SPC c) = do
  reply_chan <- newChan
  writeChan c $ MsgPing reply_chan
  readChan reply_chan

-- And in SPC.Core_Tests:

tests :: TestTree
tests =
  localOption (mkTimeout 3000000) $
    testGroup
      "SPC (core)"
      [ testCase "ping" $ do
          spc <- startSPC
          x <- pingSPC spc
          x @?= 1337
      ]

```

</details>

## A Stateful Monadic Event Loop

Most servers maintain some kind of state that is modified by the
messages they receive. Eventually SPC will have a rather complicated
notion of state, but to start out, we will simply have it track how
many ping messages it has received, and reply to pings with that
number.

To make things interesting (and ultimately, more manageable), we will
maintain the state using a monad. First, add a type repesenting the
SPC state:

```Haskell
data SPCState = SPCState
  { spcPingCounter :: Int
  }
```

Then we define an SPC monad that is a state monad with a state of type
`SPCState`, but which also supports `IO`:

```Haskell
newtype SPCM a = SPCM (SPCState -> IO (a, SPCState))
```

* Implement the `Functor`, `Applicative`, and `Monad` instances for
  `SPCM`.

<details>
<summary>Open this to see the implementation</summary>

```Haskell
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
```

</details>

We will also need the usual utility functions for managing the state:

```Haskell
get :: SPCM SPCState
put :: SPCState -> SPCM ()
```

* Implement `get` and `put`.

<details>
<summary>Open this to see the implementation</summary>

```Haskell
-- | Retrieve the state.
get :: SPCM SPCState
get = SPCM $ \state -> pure (state, state)

-- | Overwrite the state.
put :: SPCState -> SPCM ()
put state = SPCM $ \_ -> pure ((), state)
```

</details>

And we also need a function for lifting an arbitrary IO action into
`SPCM`:

```Haskell
io :: IO a -> SPCM a
```

* Implement `io`.

<details>
<summary>Open this to see the implementation</summary>

```Haskell
io m = SPCM $ \state -> do
  x <- m
  pure (x, state)
```

</details>

Finally, we need a function for running an `SPCM` operation, which
necessarily must be done in the `IO` monad:

```Haskell
runSPCM :: SPCState -> SPCM a -> IO a
```

* Implement `runSPCM`.

<details>
<summary>Open this to see the implementation</summary>

```Haskell
runSPCM state (SPCM f) = fst <$> f state
```

</details>

Now we are ready to use `SPCM`.

* Modify `startSPC` such that the loop runs inside the `SPCM` monad.

* Modify the handling of `MsgPing` such that it replies with the
  `spcPingCounter` field from the state, and then increments the field
  by one.

* Move the handling of a message into a separate function with
  signature `handleMsg :: Chan SPCMsg -> SPCM ()`.

* Add an appropriate test.

### Hints

* To read a message from a channel `c` inside `SPCM`: `io $ readChan c`.

* `succ x` produces `x+1`.

### Solution

<details>
<summary>Open this to see the implementation</summary>

```Haskell
handleMsg :: Chan SPCMsg -> SPCM ()
handleMsg c = do
  msg <- io $ readChan c
  case msg of
    MsgPing reply_chan -> do
      state <- get
      io $ writeChan reply_chan $ spcPingCounter state
      put $ state {spcPingCounter = succ $ spcPingCounter state}

startSPC :: IO SPC
startSPC = do
  c <- newChan
  let initial_state =
        SPCState
          { spcPingCounter = 0
          }
  _ <- forkIO $ runSPCM initial_state $ forever $ handle c
  pure $ SPC c

-- And a test:

tests :: TestTree
tests =
  localOption (mkTimeout 3000000) $
    testGroup
      "SPC (core)"
      [ testCase "ping" $ do
          spc <- startSPC
          x <- pingSPC spc
          x @?= 0
          y <- pingSPC spc
          y @?= 1
          z <- pingSPC spc
          z @?= 2
      ]
```

</details>

## Adding Jobs

We have now constructed the skeleton of a stateful message passing
server. Essentially all servers can start out this way, although the
monadic approach to state handling may be overkill for very simple
ones. We will now extend this skeleton with the actual job handling
functionality that is the purpose of our server. Feel free to remove
the `MsgPing` message and the related functions once you add more
interesting messages.

First we will implement a way to add jobs. Jobs are described by this
type:

```Haskell
-- | A job that is to be enqueued in the glorious SPC.
data Job = Job
  { -- | The IO action that comprises the actual action of the job.
    jobAction :: IO (),
    -- | The maximum allowed runtime of the job, counting from when
    -- the job begins executing (not when it is enqueued).
    jobMaxSeconds :: Int
  }
```

And are added by this function:

```Haskell
-- | A unique identifier of a job that has been enqueued.
newtype JobId = JobId Int
  deriving (Eq, Ord, Show)

-- | Add a job for scheduling.
jobAdd :: SPC -> Job -> IO JobId
```

The `Job` type (and its definition) should be exported, but the
`JobId` should not have its constructor exported. It must be an
abstract type from the point of view of users. This is achieved by
putting respectively `Job(..)` and `JobId` in the module export list.

When a job is enqueued, it is not executed immediately. Rather, it is
added to a list of *pending* jobs. Each job is associated with a
unique `JobId`, which is used to reference it later.

### Hints

* Extend `SPCState` with a field of type `[(JobId,Job)]` which tracks
  pending jobs, and a field of type `JobId` which contains the next
  available job identifier. Update other parts of the code as
  necessary.

* Add a synchronous message to `SPCMsg` for adding a new job. You must
  decide what payload this message should contain, but it must at
  least provide a channel for the reply, which is a `JobId`. When
  handling this message, you must add the job to the list that you
  added to the state.

* Implement `jobAdd` by using the message you added above.

* Add an appropriate test.

### Solution

<details>
<summary>Open this to see the implementation</summary>

```Haskell
data SPCState = SPCState
  { spcJobsPending :: [(JobId, Job)],
    spcJobCounter :: JobId
  }

handleMsg :: Chan SPCMsg -> SPCM ()
handleMsg c = do
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

startSPC :: IO SPC
startSPC = do
  c <- newChan
  let initial_state =
        SPCState
          { spcJobCounter = JobId 0,
            spcJobsPending = []
          }
  _ <- forkIO $ runSPCM initial_state $ forever $ handleMsg c
  pure $ SPC c

-- | Add a job for scheduling.
jobAdd :: SPC -> Job -> IO JobId
jobAdd (SPC c) job = do
  reply_chan <- newChan
  writeChan c $ MsgJobAdd job reply_chan
  readChan reply_chan

-- And a test:

tests :: TestTree
tests =
  localOption (mkTimeout 3000000) $
    testGroup
      "SPC (core)"
      [ testCase "adding job" $ do
          spc <- startSPC
          _ <- jobAdd spc $ Job (pure ()) 1
          pure ()
      ]

```

</details>

## Job Status

We will now add functionality for querying the status of a job. The
status of the job is expressed by the following two types:

```Haskell
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
```

* Add these type definitions to `SPC.Core` and also add them to the
  module export list.

Initially a job has status `JobPending`. In fact, since we have yet to
implement actual execution of jobs, that is the only constructor we
will make use of.

Your task is now to implement the function with the following
signature:

```Haskell
-- | Query the job status.
jobStatus :: SPC -> JobId -> IO JobStatus
```

### Hints

* Add a constructor to `SPCMsg` for requesting the status of a job.

* Handle the new message in `handleMsg`. If the provided `JobId` is
  found in the list of pending jobs, then the response should be
  `JobPending`, and otherwise `JobUnknown`.

* Implement the `jobStatus` function and add it to the module export
  list.

* Add an appropriate test, or modify an existing one to make use of
  `jobStatus`.

### Solution

<details>
<summary>Open this to see the implementation</summary>

```Haskell

data SPCMsg
  = ...
  | -- | Immediately reply the status of the job.
    MsgJobStatus JobId (Chan JobStatus)

handleMsg :: Chan SPCMsg -> SPCM ()
handleMsg c = do
  msg <- io $ readChan c
  case msg of
    ...
    MsgJobStatus jobid reply -> do
      state <- get
      io $ writeChan reply $ case lookup jobid $ spcJobsPending state of
        Just _ -> JobPending
        _ -> JobUnknown

-- | Add a job for scheduling.
jobAdd :: SPC -> Job -> IO JobId
jobAdd (SPC c) job = do
  reply_chan <- newChan
  writeChan c $ MsgJobAdd job reply_chan
  readChan reply_chan

-- And a test

tests :: TestTree
tests =
  localOption (mkTimeout 3000000) $
    testGroup
      "SPC (core)"
      [ testCase "adding job" $ do
          spc <- startSPC
          j <- jobAdd spc $ Job (pure ()) 1
          r <- jobStatus spc j
          r @?= JobPending
      ]


```

</details>

## Job Cancellation

This task is about implementing the following function, which cancels
a job and receives no response (even of the job was invalid).

```Haskell
-- | Asynchronously cancel a job.
jobCancel :: SPC -> JobId -> IO ()
```

When a pending job is cancelled, it should be moved to a separate list
of done jobs, which should associate a `JobId` with a `JobDoneReason`.
After a job has been cancelled, performing asking for its status
should return `DoneCancelled`.

### Hints

1. Extend `SPCState` with a list of done jobs.

2. Extend `SPCMsg` with an asynchronous message for cancelling a given
   job.

3. Handle the new message in `handleMsg`.

4. Implement `jobCancel` by moving the job (if it exists) from the
   list of pending jobs to the list of done jobs.

5. Modify the handling of the `jobStatus` message such that it replies
   `JobDone DoneCancelled` for cancelled jobs.

6. Add a test.

### Solution

<details>
<summary>Open this to see the implementation</summary>

```Haskell
data SPCMsg
  = ...
  | MsgJobCancel JobId

data SPCState = SPCState
  { ...
    spcJobsDone :: [(JobId, JobDoneReason)]
  }

handleMsg :: Chan SPCMsg -> SPCM ()
handleMsg c = do
  msg <- io $ readChan c
  case msg of
    ...
    MsgJobStatus jobid reply -> do
      state <- get
      io $ writeChan reply $ case ( lookup jobid $ spcJobsPending state,
                                    lookup jobid $ spcJobsDone state
                                  ) of
        (Just _, _) -> JobPending
        (_, Just _) -> JobDone DoneCancelled
        _ -> JobUnknown
    MsgJobCancel jobid -> do
      state <- get
      case lookup jobid $ spcJobsPending state of
        Nothing -> pure ()
        Just _ ->
          put $
            state
              { spcJobsPending = removeAssoc jobid $ spcJobsPending state,
                spcJobsDone = (jobid, DoneCancelled) : spcJobsDone state
              }

jobCancel :: SPC -> JobId -> IO ()
jobCancel (SPC c) jobid =
  writeChan c $ MsgJobCancel jobid
```

```Haskell
testCase "canceling job" $ do
  spc <- startSPC
  j <- jobAdd spc $ Job (pure ()) 1
  jobCancel spc j
  r <- jobStatus spc j
  r @?= JobDone DoneCancelled

```

</details>

## Job Waiting

This task adds support for synchronously waiting for a job to finish,
and receiving the `JobDoneReason` in response. Currently we can only
wait for jobs to be cancelled, but this will be quite useful
functionality once we implement actual job execution.

```Haskell
-- | Synchronously block until job is done and return the reason.
jobWait :: SPC -> JobId -> IO JobDoneReason
```

This is more tricky to handle than the other messages. We must send a
reply *eventually*, but we cannot do so until the job is done, which
it may not be yet. This means we must maintain a list pairing `JobId`s
with channels to send a message to once that job finishes. Then, when
a job finishes (currently only possible when it is cancelled), we must
send a response to all relevant channels.

### Hints

1. Extend `SPCMsg` with a new message for waiting on a given job to finish.

2. Extend `SPCState` with with a field of type `[(JobId, Chan
   JobDoneReason)]`.

3. Extend `handleMsg` to handle the new message added in step 1.

4. Modify `handleMsg`'s logic for job cancellation to see if anyone is
   waiting for the job to be done (by inspecting the state list added
   in step 2). If so, send `DoneCancelled` on the corresponding
   channels and remove the waiters from the list.

### Solution

<details>
<summary>Open this to see the implementation</summary>

```Haskell
data SPCMsg
  = ...
  | MsgJobWait JobId (Chan JobDoneReason)

data SPCState = SPCState
  { ...
    spcWaiting :: [(JobId, Chan JobDoneReason)]
  }

handleMsg :: Chan SPCMsg -> SPCM ()
handleMsg c = do
  msg <- io $ readChan c
  case msg of
    ...
    MsgJobCancel jobid -> do
      state <- get
      case lookup jobid $ spcJobsPending state of
        Nothing -> pure ()
        Just _ -> do
          let (waiting_for_jobid, rest) =
                partition ((== jobid) . fst) $ spcWaiting state
          forM_ waiting_for_jobid $ \(_, waiter_chan) ->
            io $ writeChan waiter_chan DoneCancelled
          put $
            state
              { spcJobsPending = removeAssoc jobid $ spcJobsPending state,
                spcJobsDone = (jobid, DoneCancelled) : spcJobsDone state,
                spcWaiting = rest
              }
    MsgJobWait jobid reply -> do
      state <- get
      case lookup jobid $ spcJobsDone state of
        Just reason -> do
          io $ writeChan reply reason
        Nothing ->
          put $ state {spcWaiting = (jobid, reply) : spcWaiting state}

jobWait :: SPC -> JobId -> IO JobDoneReason
jobWait (SPC c) jobid = do
  reply_chan <- newChan
  writeChan c $ MsgJobWait jobid reply_chan
  readChan reply_chan

```

```Haskell
testCase "canceling job" $ do
  spc <- startSPC
  j <- jobAdd spc $ Job (pure ()) 1
  jobCancel spc j
  r <- jobWait spc j
  r @?= DoneCancelled
```

</details>

## Job Execution

In this task you will add support for actually executing a job. We
will initially ignore the possibility of timeouts and crashes and
return to these concerns in later tasks.

The semantics of job execution are simple: when a job is enqueued, it
will eventually be executed. Only a single job is under execution at
any given time. If the scheduler is idle, and there are pending jobs,
a pending job should be chosen and begin execution.

Operationally, when SPC executes a job, it spawns a new thread (with
`forkIO`) which runs the `jobAction`. We call this a *job thread* When
the action is done, the job thread will send a message to SPC. Only
one job thread should be running at any given time. To support
`jobCancel` and `jobStatus`, SPC must track the currently running job
(if any).

### Hints

0. We will now have multiple ways that a job can finish, so add a
   function `jobDone :: JobId -> JobDoneReason -> SPCM ()` that
   contains the book-keeping code from in your handling of the
   cancellation message. Modify `handleEvent` to use this function.
   Ensure that your existing tests still work.

1. Extend `SPCState` with a field tracking the currently running job,
   which must contain a `JobId` and a `ThreadId`.

2. Extend `SPCMsg` with a message that is sent a job thread when its
   job is done.

3. Define a function with signature `schedule :: SPCM ()`. When
   `schedule` is executed, it checks whether a not is *not* currently
   running, and if there are any *pending* jobs. If so, it launches a
   thread as discussed above and updates the SPC state appropriately.

4. Insert a call to `schedule` at an appropriate location (such as the
   beginning of `handleMsg`).

5. Handle the message you added in step (2) in `handleMsg`. Use the
   `jobDone` function you added above.

6. Write tests. Since jobs are executed only for side effects and do
   not return any values, we need to write test jobs with observable
   side effects. The easiest way is for the test to create an `IORef`
   whose value is changed by the job.

### Solution

<details>
<summary>Open this to see the implementation</summary>

```Haskell
data SPCMsg
  = ...
  | MsgJobDone JobId

data SPCState = SPCState
  { ...
    spcChan :: Chan SPCMsg,
    spcJobRunning :: Maybe (JobId, ThreadId)
  }

schedule :: SPCM ()
schedule = do
  state <- get
  case (spcJobRunning state, spcJobsPending state) of
    (Nothing, (jobid, job) : jobs) -> do
      t <- io $ forkIO $ do
        jobAction job
        writeChan (spcChan state) $ MsgJobDone jobid
      put $
        state
          { spcJobRunning = Just (jobid, t),
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

handleMsg :: Chan SPCMsg -> SPCM ()
handleMsg c = do
  schedule
  msg <- io $ readChan c
  case msg of
    ...
    MsgJobStatus jobid reply -> do
      state <- get
      io $ writeChan reply $ case ( lookup jobid $ spcJobsPending state,
                                    spcJobRunning state,
                                    lookup jobid $ spcJobsDone state
                                  ) of
        (Just _, _, _) -> JobPending
        (_, Just (running_job, _), _)
          | running_job == jobid ->
              JobRunning
        (_, _, Just r) -> JobDone r
        _ -> JobUnknown
    MsgJobDone done_jobid -> do
      state <- get
      case spcJobRunning state of
        Just (jobid, _)
          | jobid == done_jobid ->
              jobDone jobid Done
        _ -> pure ()
    MsgJobCancel cancel_jobid -> do
      state <- get
      case spcJobRunning state of
        Just (jobid, tid) | jobid == cancel_jobid -> do
          io $ killThread tid
          jobDone jobid DoneCancelled
        _ -> pure ()
```

Test case.

```Haskell
testCase "running job" $ do
  ref <- newIORef False
  spc <- startSPC
  j <- jobAdd spc $ Job (writeIORef ref True) 1
  r <- jobWait spc j
  r @?= Done
  x <- readIORef ref
  x @?= True
```

</details>

## Handling Crashing Jobs

In this task we will handle the situation where a job throws an
exception during execution. When this occurs, the job should be
considered done, with a `JobDoneReason` of `DoneCrashed`.

### Solution

<details>
<summary>Open this to see the implementation</summary>

```Haskell

data SPCMsg
  = ...
  | MsgJobDone JobId
  | MsgJobCrashed JobId

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
      put $
        state
          { spcJobRunning = Just (jobid, t),
            spcJobsPending = jobs
          }
    _ -> pure ()

handleMsg :: Chan SPCMsg -> SPCM ()
handleMsg c = do
  schedule
  msg <- io $ readChan c
  case msg of
    ...
    MsgJobCrashed crashed_jobid -> do
      state <- get
      case spcJobRunning state of
        Just (jobid, tid) | jobid == crashed_jobid ->
          jobDone jobid DoneCrashed
        _ -> pure ()

```

Test case:

```Haskell
testCase "crash" $ do
  spc <- startSPC
  j1 <- jobAdd spc $ Job (error "boom") 1
  r1 <- jobWait spc j1
  r1 @?= DoneCrashed
  -- Ensure new jobs can still work.
  ref <- newIORef False
  j2 <- jobAdd spc $ Job (writeIORef ref True) 1
  r2 <- jobWait spc j2
  r2 @?= Done
  v <- readIORef ref
  v @?= True
```

</details>

## Handling Timeouts

In this task we will enforce the timeout contained in the `Job`
datatype. If a job runtime exceeds its timeout, it will be terminated,
and its `JobDoneReason` set to `DoneTimeout`.

The key trick is based on the observation that SPC is only able to
take action in response to a message, but a task stuck in an infinite
loop sends no messages. Therefore we will add a trivial message with
no payload (called a "tick") and arrange for a thread to exist that
does nothing but infinitely send these tick messages to SPC every
second or so.

We further augment the state field tracking the currently running job
to contain a *deadline*. When the current time (as retrieved by
`getSeconds`) exceeds the deadline, the task must be terminated.

### Hints

0. Extend `SPCMsg` with a tick message type.

1. Modify `SPCState` such that a running job is associated with a
   deadline of type `Seconds`.

2. Modify `schedule` such that when a job begins execution, its
   deadline is set to the current time plus `jobMaxSeconds` of the
   job.

3. Add a function with signature `checkTimeouts :: SPCM ()` that
   checks whether the current job has exceeded its deadlne. If so,
   destroy the job thread with `killThread` and use `jobDone` to
   register the fact that the job is now done.

4. Extend `handleMsg` to call `checkTimeouts` as appropriate and
   handle the new tick message type.

### Solution

<details>
<summary>Open this to see the implementation</summary>

```Haskell
data SPCMsg =
  ...
  | MsgTick

data SPCState = SPCState
  { ...
    spcChan :: Chan SPCMsg,
    spcJobRunning :: Maybe (JobId, Seconds, ThreadId)
  }

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
  ...
  case msg of
    ...
    MsgTick ->
      pure ()
```

Test case:

```
testCase "timeout" $ do
  spc <- startSPC
  ref <- newIORef False
  j <- jobAdd spc $ Job (threadDelay 2000000 >> writeIORef ref True) 1
  r1 <- jobStatus spc j
  r1 @?= JobRunning
  r2 <- jobWait spc j
  r2 @?= DoneTimeout,
```

</details>
