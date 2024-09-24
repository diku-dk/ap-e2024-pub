# Week 4 - Free Monads

## Suggested Reading

* [Course Notes Chapter 4](https://diku-dk.github.io/ap-notes/chapter_4.html)

  * Optional: [Introduction to Free Monads](https://serokell.io/blog/introduction-to-free-monads)

* Chapter 10 in *Programming in Haskell*.

### Going Beyond

* [Free Monads for Less part 1](http://comonad.com/reader/2011/free-monads-for-less/)
* [Free Monads for Less part 2](http://comonad.com/reader/2011/free-monads-for-less-2/)

## Exercises

In these exercises, you will construct a monadic interpreter similar in
functionality to the one you developed in week 2. This time, however, you will
implement the evaluation monad as a free monad. The fourth mandatory assignment
will ask that you then extend the evaluation monad with new effects and ways of
interpreting them.

Before you proceed with these exercises, it's important that you've read and
understood the week 4 course notes on free monads. If you haven't, now's the
time to pause and [go read](https://diku-dk.github.io/ap-notes/chapter_4.html)!

Your starting point is found in [handout/](handout/) and complete solutions are
in [solutions/](solutions/). For some of the tasks, partial or complete
solutions are also included inline below.

### Handout Structure

The handout has the following structure. These exercises will form the starting
basis of A4, so pay attention!

```
handout
├── runtests.hs
├── src
│   └── APL
│       ├── AST.hs
│       ├── Eval.hs
│       ├── InterpIO.hs
│       ├── InterpPure.hs
│       ├── Interp_Tests.hs
│       ├── Monad.hs
│       └── Util.hs
└── week4.cabal
```

- `runtests.hs`: Test runner. **Do not modify this file.**

- `week4.cabal`: Cabal build file. **Do not modify this file.**

- `src/APL/AST.hs`: AST definition. **Do not modify this file.**

- `src/APL/Eval.hs`: An incomplete evaluator corresponding to the solution to
  the week 2 exercises. 
  
- `src/APL/InterpIO.hs`: Contains the incomplete IO-based `runEvalIO`
  interpreter.
  
- `src/APL/InterpPure.hs`: Contains the incomplete pure `runEval` interpreter.
  
- `src/APL/Interp_Tests.hs`: An interpreter test suite where you will add
  plentiful tests.
  
- `src/APL/Monad.hs`: Contains all things related to the evaluation monad. Note
   that some definitions from A2 have moved from `APL.Eval` to
   `APL.Monad` in this assignment; e.g. `Val` and definitions related to the
   environment.

- `src/APL/Util.hs`: Utility functions needed for testing IO and some other
   stuff. You can safely ignore this file. **Do not modify this file.**
  
### Getting Started

- `APL.Eval`: To make testing easier, you should replace the definition of
  `eval` with your complete evaluator from your solution to A2.

  Note that doing this isn't strictly required and you can fully solve and test
  (by constructing the appropriate `EvalM` values directly or with the interface
  functions in `APL.Monad`) both these exercises and the assignment without
  replacing `eval`. But, we think it's probably nicer to have a complete version
  of `eval` that can handle all of the different expression types. It's also
  cool to see that you don't have to change your version of `eval` from
  A2 at all despite the fact that the underlying monad will change
  rather significantly.

### `Functor` and `Monad` instances for `Free e a`

- `APL.Monad`: Complete the skeleton code for the `Functor` and `Monad`
  instances of `Free e a`.
  
If you feel the need to review the course notes, feel free to first do so, but
try to complete the definitions without directly copying from the notes.

#### Hints

Keep in mind the `Functor e` constraint on each of the instances---this should
suggest to you that you'll have to (at least) use `e`'s `fmap` in each of the
instances.

#### Solution

<details>
<summary>Open this to see the answer</summary>

```Haskell
-- APL.Monad:
instance (Functor e) => Functor (Free e) where
  fmap f (Pure x) = Pure $ f x
  fmap f (Free g) = Free $ fmap (fmap f) g

instance (Functor e) => Monad (Free e) where
  Pure x >>= f = f x
  Free g >>= f = Free $ h <$> g
    where
      h x = x >>= f
```

</details>

### Defining the evaluation monad

In this task, you'll develop a free monad-based version of the `EvalM` evaluation
monad. The first thing to do is define an effect type for`EvalM`, which will
include a constructor for each kind of effect we want the `EvalM` monad to
support. To keep things simple, we'll add effects one-by-one. Let's start with a
`Reader` effect:

```Haskell
-- APL.Monad:
data EvalOp a
  = ReadOp (Env -> a)
```

To make `EvalOp` usable with `Free`, it needs a `Functor` instance.

- `APL.Monad`: Implement the `Functor` instance for `EvalOp`.

Our evaluation monad is just a free monad with effects chosen from `EvalOp`:

```Haskell
-- APL.Monad:
type EvalM a = Free EvalOp a
```

The next step is to begin building the pure interpretation function to run `EvalM`
monads.

- `APL.InterpPure`: Complete the skeleton code for `runEval`.

- `APL.Interp_Tests`: Add tests to `pureTests` to test your new effect.

#### Hints

Test your `Reader` effect by adding some tests to `pureTests` in
`APL.Interp_Tests`. It is probably easiest to construct an `Exp` whose
evaluation generates `Reader` effects, like `Let`-expressions:

```hs
testCase "Let" $
  eval' (Let "x" (Add (CstInt 2) (CstInt 3)) (Var "x"))
    @?= ValInt 5,
```

#### Solution 

<details>
<summary>Open this to see the answer</summary>


```Haskell
-- APL.Monad:
instance Functor EvalOp where
  fmap f (ReadOp k) = ReadOp $ f . k
```

```hs
-- APL.InterpPure:
runEval :: EvalM a -> a
runEval = runEval' envEmpty
  where
    runEval' :: Env -> EvalM a -> a
    runEval' _ (Pure x) = x
    runEval' r (Free (ReadOp k)) = runEval' r $ k r
```

</details>

### Extending `EvalM` with more effects

Now that we have the basic structure of `EvalM` in place, it's time to extend it
with some more effects.

#### State effects

There are two state effects:

1. You can retrieve the state.
2. You can replace the state with a new state.

We'll have to model both, so let's extend `EvalOp` appropriately:

```Haskell
-- APL.Monad:
data EvalOp a
  = ReadOp (Env -> a)
  | StateGetOp (State -> a)
  | StatePutOp State a
```

- `APL.Monad`: Extend the `Functor` instance for `EvalOp` to include the new state effects.

- `APL.InterpPure`: Extend the code for `runEval` to include support for the new
  state effects.

- `APL.Interp_Tests`: Add tests to `pureTests` to test your new effects.

#### Hints

You'll have to modify `runEval'` to take a new parameter---the state:

```hs
-- APL.InterpPure:
runEval' :: Env -> State -> EvalM a -> a
```

#### Solution 

<details>
<summary>Open this to see the answer</summary>

```Haskell
-- APL.Monad:
instance Functor (EvalOp r s) where
  fmap f (ReadOp k) = ReadOp $ f . k
  fmap f (StateGetOp k) = StateGetOp $ f . k
  fmap f (StatePutOp s m) = StatePutOp s $ f m
```

```Haskell
-- APL.InterpPure:
runEval :: EvalM a -> a
runEval = runEval' envEmpty stateInitial
  where
    runEval' :: Env -> State -> EvalM a -> a
    runEval' _ _ (Pure x) = x
    runEval' r s (Free (ReadOp k)) = runEval' r s $ k r
    runEval' r s (Free (StateGetOp k)) = runEval' r s $ k s
    runEval' r _ (Free (StatePutOp s' m)) = runEval' r s' m
```

</details>

### Some useful interfaces

Just like in A2, we write interface functions so we don't have to
manually construct`EvalM` effect terms. For example, to retrieve the
environment we re-implement `askEnv` from A2 to work with the free
monad version of `EvalM`:

```Haskell
-- APL.Monad:
askEnv :: EvalM Env
askEnv = Free $ ReadOp $ \env -> pure env
```

There are also a few useful interfaces for retrieving/putting/manipulating
state.

- Implement `getState` and `putState`. Use them to implement `modifyState`.

We also want a way to make local environments, i.e., we want to implement the
function

```Haskell
-- APL.Monad:
localEnv :: (Env -> Env) -> EvalM a -> EvalM a
localEnv f m = ...
```

This interface is a bit trickier. Somehow we have to take an `EvalM a`---which
you can think of as *stack* of effects that returns some `a`---and modify every
`ReadOp` effect within that stack using`f`. How do we do that? We're going to
need some way to traverse the stack of effects and modify them:[^1]

```Haskell
-- APL.Monad:
modifyEffects :: (Functor e, Functor h) => (e (Free e a) -> h (Free e a)) -> Free e a -> Free h a
modifyEffects _ (Pure x) = Pure x
modifyEffects g (Free e) = ...
```

- `APL.Monad`: Complete the definition of `modifyEffects`.

- `APL.Monad`: Using `modifyEffects`, implement `localEnv`.

- `APL.Interp_Tests`: Add tests to `pureTests` to test your new interfaces. Interpreting `APL`
  expressions that modify the state and/or generate local environments is
  probably a good idea, but you can also test interfaces
  directly:
  
  ```hs
  testCase "localEnv" $
    runEval
      ( localEnv (const [("x", ValInt 1)]) $
               askEnv
    )
      @?= [("x", ValInt 1)]
  ```

#### Hints

`modifyEffects` is a recursive function and uses `fmap`.

In your definition of `localEnv`, the function you pass to `modifyEffects`
should only modify `ReadOp` effects; for all other effects it should act as the
identity function.

#### Solution 

<details>
<summary>Open this to see the answer</summary>


```Haskell
-- APL.Monad:
modifyEffects :: (Functor e, Functor h) => (e (Free e a) -> h (Free e a)) -> Free e a -> Free h a
modifyEffects _ (Pure x) = Pure x
modifyEffects g (Free e) = Free $ modifyEffects g <$> g e

localEnv :: (Env -> Env) -> EvalM a -> EvalM a
localEnv f = modifyEffects g
  where
    g (ReadOp k) = ReadOp $ k . f
    g op = op

getState :: EvalM State
getState = Free $ StateGetOp $ \s -> pure s

putState :: State -> EvalM ()
putState s = Free $ StatePutOp s $ pure ()

modifyState :: (State -> State) -> EvalM ()
modifyState f = do
  s <- getState
  putState $ f s
```

</details>

### Print effects

Let's extend `EvalOp` with an effect for printing:

```Haskell
-- APL.Monad:
data EvalOp a
  = ReadOp (Env -> a)
  | StateGetOp (State -> a)
  | StatePutOp State a
  | PrintOp String a
```

- `APL.Monad`: Extend the `Functor` instance of `EvalOp` to include `PrintOp`.

We'll need to change the type of `runEval` and `runEval'` to also spit out a
list of printed strings:

```hs 
-- APL.InterpPure:
runEval :: EvalM a -> ([String], a)

runEval' :: Env -> State -> EvalM a -> ([String], a)
```

You'll also have to modify the `Pure` case of `runEval` to play nice with the
new output type. You'll also need to appropriately change the type signature of
`eval'` in `APL.Interp_Tests`.

On each `PrintOp s m` effect, `runEval` should include the string `s` in the
final output.

- `APL.Monad`: Using `PrintOp`, fill in the definition for `evalPrint`.

- `APL.InterpPure`: Extend `runEval` to support `PrintOp` effects. 

- `APL.Interp_Tests`: Add tests to `pureTests` for the `PrintOp` effect.

#### Solution 

<details> <summary>Open this to see the answer</summary>


```Haskell
-- APL.Monad:
instance Functor EvalOp where
  fmap f (ReadOp k) = ReadOp $ f . k
  fmap f (StateGetOp k) = StateGetOp $ f . k
  fmap f (StatePutOp s m) = StatePutOp s $ f m
  fmap f (PrintOp p m) = PrintOp p $ f m
  
evalPrint :: String -> EvalM ()
evalPrint p = Free $ PrintOp p $ pure ()
```


```Haskell
-- APL.InterpPure:
runEval :: EvalM a -> ([String], a)
runEval = runEval' envEmpty stateInitial
  where
    runEval' :: Env -> State -> EvalM a -> ([String], a)
    runEval' _ _ (Pure x) = ([], x)
    runEval' r s (Free (ReadOp k)) = runEval' r s $ k r
    runEval' r s (Free (StateGetOp k)) = runEval' r s $ k s
    runEval' r _ (Free (StatePutOp s' m)) = runEval' r s' m
    runEval' r s (Free (PrintOp p m)) =
      let (ps, res) = runEval' r s m
       in (p : ps, res)
```

</details>

### Error effects

The `ErrorOp` effect is for failures. When an `ErrorOp` effect is interpreted,
the computation fails and an error is returned as the result. Just like in the
previous assignments, we'll use `Either` to be able represent both failure and
success results and we'll use the existing `Error` type for errors.

When `runEval` interprets an `ErrorOp e` effect, the result of the
entire computation should be `Left e`. For example,
```
> runEval $ Free $ ErrorOp "Oh no!"
([],Left "Oh no!")
```

Let's extend `EvalOp` with error effects:

```Haskell
-- APL.Monad:
data EvalOp a
  = ReadOp (Env -> a)
  | StateGetOp (State -> a)
  | StatePutOp State a
  | PrintOp String a
  | ErrorOp Error
```

- `APL.Monad`: Extend the `Functor` instance of `EvalOp` to include `ErrorOp`.

- `APL.Monad`: Using `ErrorOp`, fill in the definition for `failure`.

Change the type of `runEval` and `runEval'` to return an `Either Error a` type:

```Haskell
-- APL.Monad:
runEval :: EvalM a -> ([String], Either Error a)

runEval' :: Env -> State -> EvalM a -> ([String], Either Error a)
```

You'll also need to update the `Pure x` case for `runEval'` to play nice with
the new return type. You'll also need to appropriately change the type signature
of `eval'` in `APL.Interp_Tests`.

- `APL.InterpPure`: Extend `runEval` to support `ErrorOp` effects. 

- `APL.Interp_Tests`: Add tests to `pureTests` for the `ErrorOp` effect.

#### Solution 

<details>
<summary>Open this to see the answer</summary>


```Haskell
-- APL.Monad:
instance Functor EvalOp where
  fmap f (ReadOp k) = ReadOp $ f . k
  fmap f (StateGetOp k) = StateGetOp $ f . k
  fmap f (StatePutOp s m) = StatePutOp s $ f m
  fmap f (PrintOp p m) = PrintOp p $ f m
  fmap _ (ErrorOp e) = ErrorOp e

failure :: String -> EvalM a
failure = Free . ErrorOp

runEval :: EvalM a -> ([String], Either Error a)
runEval = runEval' envEmpty stateInitial
  where
    runEval' :: Env -> State -> EvalM a -> ([String], Either Error a)
    runEval' _ _ (Pure x) = ([], pure x)
    runEval' r s (Free (ReadOp k)) = runEval' r s $ k r
    runEval' r s (Free (StateGetOp k)) = runEval' r s $ k s
    runEval' r _ (Free (StatePutOp s' m)) = runEval' r s' m
    runEval' r s (Free (PrintOp p m)) =
      let (ps, res) = runEval' r s m
       in (p : ps, res)
    runEval' _ _ (Free (ErrorOp e)) = ([], Left e)
```

`APL.InterpPure`:
```Haskell
```

</details>

### Interpreting effects

Since we've only changed the underlying workings of `EvalM`---and not its
interface---your `eval` function from A2 (which you should use to
replace the version of `eval` given in the handout in the `APL.Eval` module if
you haven't) works without modification.

The important thing to understand here is that `eval` now produces a free
monad-based stack of uninterpreted effects; it doesn't actually carry out the
effects and only produces the uninterpreted structure.

So what actually does the computation/interprets the stack of effects?  The
interpreters of course! So far, we've only worked on the pure interpreter
`runEval` in `APL.InterpPure`. Let's try it out in GHCi:

```
> m =  eval $ Let "x" (Add (CstInt 1) (CstInt 2)) $ Print ("The value of x is") (Var "x")
> runEval m
(["The value of x is: 3"], Right (ValInt 3))
```

Now,`runEval` is just one interpretation of `eval e`'s effect stack. The
`APL.InterpIO` module contains another interpreter, `runEvalIO`.  This
interpreter has IO access and so can generate side effects.

The version of `runEvalIO` included in the handout mimics the workings of
`runEval` so far---now's the time to uncomment the code in
`APL.InterpIO`---except that it is missing a case for `PrintOp`.

Instead of returning the printed string in the first component of our output,
the `runEvalIO` interpretation of a `PrintOp` effect should print to the
terminal instead. For example,

```
> runEvalIO envEmpty m
The value of x is: 3
Right (ValInt 3)
```

Notice that this operation is reflected in the type of `runEvalIO`:

```hs
-- APL.InterpIO:
runEvalIO :: EvalM a -> IO (Either Error a)
```

There's no need to return a list of printed statements (the `[String]` component
in the return of `runEval`) because `PrintOp` effects will be directly printed
to the terminal as a side effect.

Also notice that in both the `runEval` and `runEvalIO` examples above we ran the
interpreters on *the same* `m` computation. The only thing that changed is
*how we interpreted* `m`.

The point is that free monads afford us the flexibility to construct a
computation just once and then interpret the (effects of) the computation in
different ways. Pretty cool!

- `APL.InterpIO`: Add support for `PrintOp` effects to `runEvalIO'`.

#### Hints

You can use `putStrLn` to print strings to the terminal.

#### Solution

<details>
<summary>Open this to see the answer</summary>


```hs
-- APL.InterpIO:
runEvalIO :: EvalM a -> IO (Either Error a)
runEvalIO = runEvalIO' envEmpty stateInitial
  where
    runEvalIO' :: Env -> State -> EvalM a -> IO (Either Error a)
    runEvalIO' _ _ (Pure x) = pure $ pure x
    runEvalIO' r s (Free (ReadOp k)) = runEvalIO' r s $ k r
    runEvalIO' r s (Free (StateGetOp k)) = runEvalIO' r s $ k s
    runEvalIO' r _ (Free (StatePutOp s' m)) = runEvalIO' r s' m
    runEvalIO' r s (Free (PrintOp p m)) = do
      putStrLn p
      runEvalIO' r s m
    runEvalIO' _ _ (Free (ErrorOp e)) = pure $ Left e
```

</details>

### Testing `runEvalIO`

To test IO-based interpretation of effects, a special function `captureIO` is
provided for you in the `APL.Util` module:

```hs
captureIO :: [String] -> IO a -> IO ([String], a)
```

You use `captureIO` like this: `captureIO inputs m`, where `inputs` is a list of
inputs (lines) you'd like to write to `stdin` during the execution of the `m`
action. `captureIO` runs `m` using `inputs` as input to `stdin` and captures
any output to `stdout`, returning it as a list of strings (and also returns the
result of the computation itself).

We can use `captureIO` to test an IO-based interpretation of a `Print`-expression
like so:

```hs
-- APL.Interp_Tests
testCase "print" $ do
     let s1 = "Lalalalala"
         s2 = "Weeeeeeeee"
     (out, res) <-
       captureIO [] $
         runEvalIO $ do
           evalPrint s1
           evalPrint s2
     (out, res) @?= ([s1, s2], Right ())
```

For print effects, we don't have any input to `stdin` so we just feed `captureIO`
an empty list (`[]`).  In the assignment, you'll add additional effects that do
read from `stdin` and will have to add inputs for `captureIO` to test these
additional effects.

- `APL.Interp_Tests`: Add tests to `ioTests` for the `PrintOp` effect.


[^1]: This is an inefficient way to implement local environments (since you have
    to traverse over the entire effect stack each time); a better way would be
    to add a local environment effect. We do it this way for pedagogical
    reasons---implementing `modifyEffects` forces you to think about what the
    effect stack looks like and how to traverse it.
