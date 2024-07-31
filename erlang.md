# Translating from Erlang to Haskell

<img align="right" src="https://github.com/user-attachments/assets/f935aa00-315a-4365-b44d-ca29144b924d">


AP previously used the [Erlang programming
language](http://erlang.org/) for some course components. In 2024, we
use only Haskell. If you previously followed the course, but didn't
pass, and are attending the exam this year, you will need to adapt
your knowledge about concurrency from Erlang to Haskell. The best way
to do so is to follow the course, attend the lectures, and complete
the assignments.  But as a special help, this *work-in-progess*
document describes how to translate aspects of concurrent Erlang to
concurrent Haskell.

## Basics

We use the Haskell modules
[Control.Concurrent](https://hackage.haskell.org/package/base-4.20.0.1/docs/Control-Concurrent.html)
and
[Control.Concurrent.Chan](https://hackage.haskell.org/package/base-4.20.0.1/docs/Control-Concurrent-Chan.html).
The latter is implicitly re-exported by the former. The following
import should give you everything you need in most cases:

```Haskell
import Control.Concurrent
  ( Chan,
    ThreadId,
    forkIO,
    newChan,
    readChan,
    writeChan,
  )
```

## Processes and Threads

In Erlang, *processes* communicate by directly sending asynchronous
messages to each other. In Haskell, the equivalent of an Erlang
process is a *thread*. To create a new thread in Haskell, we use the
`forkIO` function, similar to using `spawn` in Erlang for creating
processes. The `forkIO` function has the following type:

```Haskell
forkIO :: IO () -> IO ThreadId
```

In other words, to create a thread we pass `forkIO` an action of type
`IO ()`, meaning a monadic computation in the `IO` monad. Typically,
this will be some kind of potentially infinite loop that receives and
handles messages, just as in Erlang. The thread will continue to run
until this action terminates.

The `forkIO` function returns a `ThreadId` that can be used for
interacting with the thread in low level ways, although we will not
make much use of that in AP. Instead, we will communicate using
channel-based messaging.

## Channels and messages

In Erlang, if we have a process ID, we can send a message to that
process. In Haskell, communication is done via *channels*. A channel is
created using the `newChan` action:

```Haskell
newChan :: IO (Chan a)
```

The `newChan` action produces a channel that can be used for sending
and receiving messages of type `a`. The precise type of `a` will be
inferred by the compiler. This is also a deviation from Erlang, where
messages are untyped.

Whenever we create a thread, we will also create a channel through which we
can communicate with the thread, as shown in below (assuming the existence of a
function `threadLoop`):

```Haskell
do c <- newChan
   forkIO $ threadLoop c
   ...
```

Now, both the new thread and we have a reference to the channel (`c`).

Messages can be both read and written to a channel, corresponding to
receiving and sending messages, using the following two functions:

```Haskell
writeChan :: Chan a -> a -> IO ()
readChan :: Chan a -> IO a
```

Conceptually, a channel is an unbounded queue of messages. Writing to
a channel is an asynchronous operation - it immediately and always
succeeds. Reading from a channel retrieves the oldest message in the
channel. If the channel is empty, reading blocks until a message is
available.

**Single-reader principle:** we adopt the rule that a channel may have
only a *single* reader, meaning only a single thread is allowed to
call `readChan` on any given channel. This is typically the thread
that we created the channel for. This is not enforced by the Haskell
type system, and there are indeed forms of concurrent programming
that are more flexible, but they are outside the scope of this note.

It is perfectly acceptable (and often necessary) for a channel to have
multiple writers.

If we call `readChan` on a channel where we hold the only reference
(meaning we would in principle wait forever), the Haskell runtime
system will raise an exception that will cause the thread to be
terminated. This is a natural and safe way to shut down a thread that
is no longer necessary, assuming the thread does not hold resources
(e.g., open files) that must be manually closed. Handling such cases is
outside the scope of this note.

Thus, we have the following correspondences between Erlang and
Haskell:

* **Sending messages:** In Erlang, we send a message (to a process)
  with the operator `!`.  For example, `Pid ! Msg` sends `Msg` to the
  process identified by `Pid`. In Haskell, we send a message (to a
  process via a channel) with the function `writeChan`.

* **Receiving messages:** In Erlang, the `recieve ... end`-expression
  is used for receiving messages. It not only waits for a message but
  also performs pattern matching on the received message to decide on
  the appropriate action. To do the same in Haskell, we first use the
  `readChan` function to receive a massage, and then do pattern
  matching with a `case`-expression.


### Basic Example

The following Erlang example:

```erlang
ex1() ->
  C = spawn(fun threadLoop/0),
  C ! 0,
  C ! 1.

threadLoop() ->
  Msg = receive N -> N end,
  io:format("Got message ~p~n", [Msg]),
  threadLoop().
```

Can thus be translated to the following Haskell code:

```Haskell
ex1 :: IO ()
ex1 = do
  c <- newChan
  _ <- forkIO $ threadLoop c -- Ignore return value.
  writeChan c 0
  writeChan c 1

threadLoop :: Chan Int -> IO ()
threadLoop c = do
  msg <- readChan c
  putStrLn $ "Got integer: " <> show msg
  threadLoop c
```

## Remote procedure calls (RPC)

Just as in Erlang, the Haskell message passing facility is
asynchronous. To implement synchronous (RPC) calls, where we wait for
a response after sending a message, we need to invent a bit of
machinery on top. The way we make it work is by creating a new channel
that is used for transmitting the result. This channel is then sent
along as part of the message.

The starting point (and always good practice) is to define an explicit
type for the messages we would like to send.

```Haskell
data Msg = MsgInc (Chan Int) Int
```

We then define our thread loop as follows:

```Haskell
threadLoop :: Chan Msg -> IO ()
threadLoop c = do
  msg <- readChan c
  case msg of
    MsgInc reply_chan x ->
      writeChan reply_chan (x + 1)
  threadLoop c
```

Given a handle to a channel of type `Chan Msg`, we can then send a
message, and wait for a response, as follows:

```Haskell
performRPC :: Chan Msg -> Int -> IO Int
performRPC c x = do
  reply_chan <- newChan
  writeChan c $ MsgInc reply_chan x
  readChan reply_chan
```

And tying it all together:

```Haskell
ex2 :: IO ()
ex2 = do
  c <- newChan
  _ <- forkIO $ threadLoop c
  print =<< performRPC c 0
  print =<< performRPC c 1

```



## Counter Server Example

Let's consider a more comprehensive Erlang example, where we use a
process to create a server that maintains a counter:

```erlang
counter()         -> spawn(fun () -> counter_loop(0) end).
incr(Cid)         -> request_reply(Cid, incr).
decr_with(Cid, N) -> request_reply(Cid, {decr, N}).
get_value(Cid)    -> request_reply(Cid, get_value).

request_reply(Pid, Request) ->
    Pid ! {self(), Request},
    receive
        {Pid, Response} -> Response
    end.

counter_loop(State) ->
    receive
      {From, incr} ->
        {NewState, Res} = {State + 1, ok},
        From ! {self(), Res},
        counter_loop(NewState);
      {From, {decr, N}} ->
        {NewState, Res} = {State - N, ok},
        From ! {self(), Res},
        counter_loop(NewState);
      {From, get_value} ->
        {NewState, Res} = {State, {ok, State}},
        From ! {self(), Res},
        counter_loop(NewState)
    end.
```

To translate this example to Haskell, the first step is to make a type
for the messages that will be send to the server. Again, we use the
pattern where we make constructor for each kind of message, and the
last argument for each constructor is a channel for sending back the
response:

```Haskell
data Msg = Incr     (Chan ())
         | Decr Int (Chan ())
         | GetValue (Chan Int)
```

Here we use the Haskell type `()` (unit) in lieu of the Erlang atom `ok`.

Next, we declare a type alias `Counter` for representing a counter
server, here just the input channel, and a function for creating a new
counter server:

```Haskell
type Counter = Chan Msg

counter :: IO Counter
counter = do
  input <- newChan
  _ <- forkIO $ counterLoop input 0
  return input
```

Like in the previous section, we define a function to abstract the
communication pattern where we send a message and then wait for an
reply:

```haskell
requestReply :: Counter -> (Chan a -> Msg) -> IO a
requestReply cnt con = do
  reply_chan <- newChan
  writeChan cnt $ con reply_chan
  readChan reply_chan
```

Note that the second argument of the `requesReply` function is a
*function* that constructs a `Msg` value.

Now we can use the `requestReply` function to define the three API
functions `incr`, `decrWith` and `getValue` for a counter server:

```haskell
incr cnt = requestReply cnt Incr
decrWith cnt n = requestReply cnt $ Decr n
getValue cnt = requestReply cnt GetValue
```

Finally, we define the internal server loop function:

```Haskell
counterLoop input state = do
  msg <- readChan input
  case msg of
    Incr from -> do
      let (newState, res) = (state + 1, ())
      writeChan from res
      counterLoop input newState
    Decr n from -> do
      let (newState, res) = (state - n, ())
      writeChan from res
      counterLoop input newState
    GetValue from -> do
      let (newState, res) = (state, state)
      writeChan from res
      counterLoop input newState
```


## Timeouts

The channel abstraction does not directly support timeouts for RPC
calls. However, we can build our own support for timeouts. The
technique we employ is to allow the reply to be either the intended
value *or* a special timeout value. When we perform an RPC, we then
also launch a new thread that sleeps for some period of time, then
write the timeout value to the channel. If the non-timeout response is
the first to arrive, then the timeout value is ignored and harmless.

First we must import the `threadDelay` function.

```Haskell
import Control.Concurrent (threadDelay)
```

Then we define a type `Timeout` with a single value `Timeout`.

```Haskell
data Timeout = Timeout
```

Then we define a message type (in this case polymorphic in `a`) where
the reply channel accepts messages of type `Either Timeout a`.

```Haskell
data Msg a = MsgDoIt (Chan (Either Timeout a)) (IO a)
```

A `Msg a` denotes a request to perform some impure operation `IO a`
(perhaps a network request), then reply with the resulting value of
type `a`.

We can use this to build a facility for performing an action with a
timeout:

```Haskell
actionWithTimeout :: Int -> IO a -> IO (Either Timeout a)
actionWithTimeout seconds action = do
  reply_chan <- newChan
  _ <- forkIO $ do -- worker thread
    x <- action
    writeChan reply_chan $ Right x
  _ <- forkIO $ do -- timeout thread
    threadDelay (seconds * 1000000)
    writeChan reply_chan $ Left Timeout
  readChan reply_chan
```

You will note that this is not a server in the usual sense, as it does
not loop: it simply launches two threads.

One downside of this function is that the worker thread (the one that
runs `action`, and might take too long) is not terminated after the
timeout. This is a problem if it is, for example, stuck in an infinite
loop that consumes ever more memory. To fix this, we can have the
timeout thread explicitly kill the worker thread. First we have to
import the `killThread` function.

```Haskell
import Control.Concurrent (killThread)
```

Then we can use it as follows.

```Haskell
actionWithTimeout2 :: Int -> IO a -> IO (Either Timeout a)
actionWithTimeout2 seconds action = do
  reply_chan <- newChan
  worker_tid <- forkIO $ do
    -- worker thread
    x <- action
    writeChan reply_chan $ Right x
  _ <- forkIO $ do
    -- timeout thread
    threadDelay (seconds * 1000000)
    killThread worker_tid
    writeChan reply_chan $ Left Timeout
  readChan reply_chan
```

Note that killing a thread is a dangerous operation in general. It may
be the case that the worker thread is stuck in some loop or waiting
for a network request, in which case it is harmless, but killing it
may also leave some shared state in an unspecified state. We will
(hopefully) not encounter such cases in AP, but it is something to be
aware of in the future.
