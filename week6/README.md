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
resources. A job is any Haskell computation in the `IO` monad,
evaluated for its side effects. We can imagine using a job scheduler
to enqueue thousands of jobs for downloading various files from the
Internet, with the scheduler taking care of ensuring only a limited
number of jobs run concurrently, handling timeouts, and so on.

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

The `forever` function is a small convenience functions for writing
infinite monadic loops.

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
