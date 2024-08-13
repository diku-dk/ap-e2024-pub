# Week 4 - Free Monads

## Slides and Material

## Suggested Reading

* [Course Notes Chapter 4](https://diku-dk.github.io/ap-notes/chapter_4.html)

## Exercises

In these exercises, you will construct a monadic interpreter similar in
functionality to the one you developed in week 2. This time, however, you will
implement the evaluation monad in terms of a free monad. The fourth mandatory
assignment will ask that you then extend the evaluation monad with new effects
and ways of interpreting them.

Before you proceed with these exercises, it's important that you've read and
understood the week 4 course notes on free monads. If you haven't, now's the
time to pause here and go read!

Your starting point is found in [handout/](handout/) and complete solutions are
in [solutions/](solutions/). For some of the tasks, partial or complete
solutions are also included inline below.

### Handout Structure
The handout has the following structure. This will be the same structure as the
assignment (indeed, these exercises will form the starting basis of the
assignment), so pay attention!

```
src
└── APL
    ├── AST.hs
    ├── Eval.hs
    ├── InterpIO.hs
    ├── InterpPure.hs
    ├── Interp_Tests.hs
    └── Monad.hs
```

- `APL.Eval`: Contains the `eval :: Exp -> EvalM Val` function to create an
evaluation computation from an `Exp`. **You should put your evaluation function
(along with any necessary helper functions) from assignment 2 here.**
          
- `APL.InterpPure`: Contains the `runEval` interpreter.  The "Pure" in the name
comes from the fact that this interpreter is side-effect free.

- `APL.InterpIO`: Contains the `runEvalIO` interpreter. The "IO" in the name
comes from the fact this interpreter runs in the IO monad and hence can generate
side effects.

- `APL.Interp_Tests.hs`: Put your tests for both `runEval` and `runEvalIO` here.

- `APL.Monad`: Contains the definition of the `Free` monad and the `EvalOp`
effects, and along with many useful interfaces that you will have to complete.

**Note**: the `APL.Monad` contains a definition of the `State` type that differs
from what you did in assignment 2 since we will no longer track printed strings
in the state. Since your `eval` function should be agnostic to the specifics of
`State` (aside from `evalKvGet` and `evalKvPut`), this shouldn't matter (we will
redfine `evalKvGet` and `evalKvPut` in this assignment).

The `APL` module already contains various definitions that should be
familiar to you from the prior weeks. It also contains a definition
of the free monad: `Free e a`. To start,

### Getting Started

- `APL.Monad`: Complete the skeleton code for the `Functor` and `Monad`
  instances of `Free e a`.
  
  If you feel the need to review the course notes, feel free to first do so, but
  try to complete the definitions without directly copying from the notes.

#### Hints

Keep in mind the `Functor e` constraint on each of the instances--this should
suggest to you that you'll have to (at least) use `e`'s `fmap` in each of the
instances.

#### Solution

<details>
<summary>Open this to see the answer</summary>

`APL.Monad`:
```Haskell
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

In this task, you'll develop a free monad-based version of the `EvalM` evaluaion
monad. The first thing to do is define an effect type for`EvalM`, which will
include a constructor for each kind of effect operation we want the `EvalM`
monad to support.  To keep things simple, we'll add effect one-by-one. Let's
start with a `Reader` effect:

`APL.Monad`:
```Haskell
data EvalOp a
  = ReadOp (Env -> a)
```

To make `EvalOp` usable with `Free`, it needs a `Functor` instance.

- `APL.Monad`: Implement the `Functor` instance for `EvalOp`.

Now thaw that `EvalOp` is a functor, we can define `EvalM` in terms of it:

`APL.Monad`:
```Haskell
type EvalM a = Free EvalOp a
```

The next step is to begin building the pure interpretation function to run `EvalM`
monads.

- `APL.InterpPure`: Complete the skeleton code for `runEval`.


#### Solution 

<details>
<summary>Open this to see the answer</summary>

`APL.Monad`:
```Haskell
instance Functor EvalOp where
  fmap f (ReadOp k) = ReadOp $ f . k
```

`APL.InterpPure`:
```hs
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
with some more functionality.

#### State
Unlike with`Reader`, there are two state effects:

1. You can retrieve the state.
2. You can replace the state with a new state.

We'll have to model both, so let's extend `EvalOp` appropriately:

`APL.Monad`:
```Haskell
data EvalOp a
  = ReadOp (Env -> a)
  | StateGetOp (State -> a)
  | StatePutOp State a
```

- `APL.Monad`: Extend the `Functor` instance for `EvalOp` to include the new state effects.

- `APL.InterpPure`: Extend the code for `runEval` to include the new state effects.


#### Hints

You'll have to modify `runEval'` to take a new parameter--the state:

`APL.InterpPure`:
```
runEval' :: Env -> State -> EvalM a -> a
```

#### Solution 

<details>
<summary>Open this to see the answer</summary>

`APL.Monad`:
```Haskell
instance Functor (EvalOp r s) where
  fmap f (ReadOp k) = ReadOp $ f . k
  fmap f (StateGet k) = StateGet $ f . k
  fmap f (StatePut s m) = StatePut s $ f m
```

`APL.InterpPure`:
```Haskell
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

Just like in assignment 2, we don't always want to construct instances of
`EvalM` manually. Instead, we write interface fucntions. For example, to
retrieve the environment we re-implement `askEnv` from assignment 2 to work with
the free monad versin of `EvalM`:

`APL.Monad`:
```Haskell
askEnv :: EvalM Env
askEnv = Free $ ReadOp $ \env -> pure env
```

- `APL.Monad`: Fill in the definition of `evalKvGet` and `evalKvPut`.

We also want a way to make local environments, i.e. implement the function

`APL.Monad`:
```Haskell
localEnv :: (Env -> Env) -> EvalM a -> EvalM a
localEnv f m = ...
```

This function is a bit trickier. Somehow we have to take an `EvalM`---which is a
*stack* of effects---and modify every `ReadOp` effect within that stack
using`f`. How do we do that? We need some way to traverse the stack of effects
and modify them.

Recall the definition of `Free e a`:
```Haskell
data Free e a
  = Pure a
  | Free (e (Free e a))
```
Our objective is to apply a function to modify each effect `e` in the stack. That is,
we want to change each `e` to `f`. But, we don't want to just change the top `e` but rather
every `e` in the stack, so what we really want is a function `e (Free e a) -> f (Free f a)`:

`APL.Monad`:
```Haskell
modifyEffects :: (Functor e, Functor f) => (e (Free e a) -> f (Free e a)) -> Free e a -> Free f a
modifyEffects _ (Pure x) = Pure x
modifyEffects g (Free e) = ...
```

- `APL.Monad`: Complete the definition of `modifyEffects`.

- `APL.Monad`: Using `modifyEffects`, implement `localEnv`.


#### Hints

`modifyEffects` is a recursive function and uses `fmap`.

In your definition of `localEnv`, the function you pass to `modifyEffects`
should only modify `ReadOp` effects; for all other effects it should act as the
identity function.

#### Solution 

<details>
<summary>Open this to see the answer</summary>

`APL.Monad`:
```Haskell
modifyEffects :: (Functor e, Functor f) => (e (Free e a) -> f (Free e a)) -> Free e a -> Free f a
modifyEffects _ (Pure x) = Pure x
modifyEffects g (Free e) = Free $ modifyEffects g <$> g e

localEnv :: (Env -> Env) -> EvalM a -> EvalM a
localEnv f = modifyEffects g
  where
    g (ReadOp k) = ReadOp $ k . f
    g op = op

getState :: EvalM State
getState = Free $ StateGet $ \s -> pure s

putState :: State -> EvalM ()
putState state = Free $ StatePut state $ pure ()

modifyState :: (State -> State) -> EvalM ()
modifyState f = do
  state <- getState
  putState $ f state
```

</details>

### Print effects

Let's extend `EvalOp` with an effect for printing:

`APL.Monad`:
```Haskell
data EvalOp a
  = ReadOp (Env -> a)
  | StateGetOp (State -> a)
  | StatePutOp State a
  | PrintOp String a
```

- `APL.Monad`: Extend the `Functor` instance of `EvalOp` to include `PrintOp`.

- `APL.Monad`: Using `PrintOp`, fill in the definition for `evalPrint`.

- `APL.InterpPure`: Extend `runEval` to support `PrintOp` effects. 

#### Hints

You'll have to change the type of `runEval` and `runEval'` to also spit out a log:

`APL.InterpPure`:
```hs 
runEval :: EvalM a -> ([String], a)

runEval' :: Env -> State -> EvalM a -> ([String], a)
```

On each `PrintOp s m` effect, `runEval` should include thbe string `s` in the
final output. Make sure that your log is in the right order!

You'll also have to modify the `Pure` case of `runEval` to play nice with the new
output type.

#### Solution 

<details>
<summary>Open this to see the answer</summary>

`APL.Monad`:
```Haskell
instance Functor EvalOp where
  fmap f (ReadOp k) = ReadOp $ f . k
  fmap f (StateGetOp k) = StateGetOp $ f . k
  fmap f (StatePutOp s m) = StatePutOp s $ f m
  fmap f (PrintOp p m) = PrintOp p $ f m
  
evalPrint :: String -> EvalM ()
evalPrint p = Free $ PrintOp p $ pure ()
```

`APL.InterpPure`:
```Haskell
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

When `runEval` inteprets an `ErrorOp e` effect, the result of the
entire computation should be `Left e`. For example,
```
> runEval $ Free $ ErrorOp "Oh no!"
([],Left "Oh no!")
```

Let's extend `EvalOp` with an effect for error effects:

`APL.Monad`:
```Haskell
data EvalOp a
  = ReadOp (Env -> a)
  | StateGetOp (State -> a)
  | StatePutOp State a
  | PrintOp String a
  | ErrorOp Error
```

- `APL.Monad`: Extend the `Functor` instance of `EvalOp` to include `ErrorOp`.

- `APL.Monad`: Using `ErrorOp`, fill in the definition for `failure`.

- `APL.InterpPure`: Extend `runEval` to support `ErrorOp` effects. 

#### Hints

You'll need to change the type of `runEval` and `runEval'` to return an `Either
Error a` type:

`APL.Monad`:
```Haskell
runEval :: EvalM a -> ([String], Either Error a)

runEval' :: Env -> State -> EvalM a -> ([String], Either Error a)
```

You'll also need to update the `Pure x` case for `runEval'` to play nice with
the new return type.

#### Solution 

<details>
<summary>Open this to see the answer</summary>

`APL.Monad`:
```Haskell
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
interface---your `eval` function from assignment 2 (which you should plop into
the `APL.Eval` module if you haven't) works without modification.

The important thing to understand here is that `eval` now produces a free
monad-based stack of uninterpreted effects; it doesn't actually carry out the
effects and only produces the uninterpreted structure.

So what actually does the computation/interprets the stack of effects?  The
interpreters of course! So far, we've only worked on `runEval`.  Let's try it
out in GHCi:

```
> m =  eval $ Let "x" (Add (CstInt 1) (CstInt 2)) $ Print ("The value of x is") (Var "x")
> runEval envEmpty stateInitial m
(["The value of x is: 3"], Right (ValInt 3))
```

But `runEval` is just one interpretation of `eval e`'s effect stack. The 
`APL.InterpIO` contains another interpreter, `runEvalIO` for defining
a different side-effect based interpreter that has IO access.

The verison of `runEvalIO` include in the handout mimics the workings of
`runEval` so far , except that its workings on the `PrintOp` is undefined.

Instead of returning the printed string in the first component of our output,
the `runEvalIO` interpretation of a `PrintOp` effect should print to the
terminal instead. For example,

```
> runEvalIO envEmpty m
The value of x is: 3
Right (ValInt 3)
```

Notice that this operation is reflected in the type of `runEvalIO`

`APL.InterpIO`:
```hs
runEvalIO :: EvalM a -> IO (Either Error a)
```

There's no need to return a log (the `[String]` component in the return of
`runEval`) because `PrintOp` effects will be directly printed to the terminal
(hence the `IO`-based return).

Notice that in both the `runEval` and `runEvalIO` examples we used *the same*
`m`, i.e., the exact same monadic value. The only thing that changed is *how we
interpreted `m`*.  That is, we can construct our computation once using `eval`
and the interpret the effects of the computation in different ways, without
recreating the computation or defining a different evaluator for each effect
interpretation. Pretty cool!

- `APL.InterpIO`: Add support for `PrintOp` effects to `runEvalIO'`.

#### Hints

You can use `putStrLn` to print strings to the terminal.

#### Solution 

<details>
<summary>Open this to see the answer</summary>

`APL.InterpIO`:
```hs
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
