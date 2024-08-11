# Week 4 - Free Monads

## Slides and Material

## TBA

## Suggested Reading

## Exercises

In these exercises, you will construct a monadic interpreter similar in
functionality to the one you developed in week 2. This time, however, you will
implement the evaluation monad in terms of a free monad. The fourth mandatory
assignment will ask that you then extend the evaluation monad with new effects
and ways of interpreting them.

Before you proceed with these exercises, it's important that you've read and
understood the week 4 course notes on free monads. If you haven't, now's the
time to pause here and go read!

Your starting point is found in [handout/](handout/) and complete
solutions are in [solutions/](solutions/). For some of the tasks,
partial or complete solutions are also included inline below. In terms
of modules, the structure is identical to weeks 1 and 2.

### Getting Started

The `APL` module already contains various definitions that should be
familiar to you from the prior weeks. It also contains a definition
of the free monad: `Free e a`. To start,

- Complete the skeleton code for the `Functor` and `Monad`
  instances of `Free e a`.

#### Hints

Keep in mind the `Functor e` constraint on ecah of the instances--this should
suggest to you that you'll have to (at least) use `e`'s `fmap` in each of the
instances.

#### Solution

<details>
<summary>Open this to see the answer</summary>

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

```Haskell
data EvalOp r a
  = ReadOp (r -> a)
```

To make `EvalOp` usable with `Free`, it needs a `Functor` instance.

- Implement the `Functor` instance for `EvalOp`.

Now thaw that `EvalOp` is a functor, we can define `EvalM` in terms of it:

```Haskell
type EvalM a = Free (EvalOp Env) a
```

The next step is to begin building our interpretation function to run `EvalM`
monads.

- Complete the skeleton code for `runEval`.


#### Solution 

<details>
<summary>Open this to see the answer</summary>

```Haskell
instance Functor (EvalOp r) where
  fmap f (ReadOp k) = ReadOp $ f . k

runEval :: Env -> EvalM a -> a
runEval _ (Pure x) = x
runEval r (Free (ReadOp k)) = runEval r s $ k r
```

</details>

### Extending `EvalM` with more effects

Now that we have the basic structure of `EvalM` in place, it's time to extend it
with some more functionality. We also want to add support for state
effects. Unlike with`Reader`, there are two state effects:

1. You can retrieve the state.
2. You can replace the state with a new state.

We'll have to model both, so we extend `EvalOp` appropriately (along with a new
type parameter `s` to abstract over the type of the state):

```Haskell
data EvalOp r s a
  = ReadOp (r -> a)
  | StateGet (s -> a)
  | StatePut s a
```

- Fix the definition of `EvalM` to use `State` (already defined) for the `s`
  parameter.

- Extend the `Functor` instance for `EvalOp` to include the new state effects.

- Extend the code for `runEval` to include the new state effects.


#### Hints

You'll have to modify `runEval` to take a new parameter---the initial state:

```Haskell
runEval :: Env -> State -> EvalM a -> a
```

#### Solution 

<details>
<summary>Open this to see the answer</summary>

```Haskell
type EvalM a = Free (EvalOp Env State) a

instance Functor (EvalOp r s) where
  fmap f (ReadOp k) = ReadOp $ f . k
  fmap f (StateGet k) = StateGet $ f . k
  fmap f (StatePut s m) = StatePut s $ f m
  
runEval :: Env -> State -> EvalM a -> a
runEval _ _ (Pure x) = (mempty, Right x)
runEval r s (Free (ReadOp k)) = runEval r s $ k r
runEval r s (Free (StateGet k)) = runEval r s $ k s
runEval r _ (Free (StatePut s' m)) = runEval r s' m
```

</details>

### Some useful interfaces

Just like in assignment 2, we don't always want to construct instances of
`EvalM` manually. For example, to retrieve the environment we re-implement
`askEnv` from assignment 2 to work with the free monad versin of `EvalM`:

```Haskell
askEnv :: EvalM Env
askEnv = Free $ ReadOp $ \env -> pure env
```

- Fill in the definition of `getState`, `putState`, and `modifyState`.

We also want a way to make local environments, i.e. implement the function

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

```Haskell
modifyEffects :: (Functor e, Functor f) => (e (Free e a) -> f (Free f a)) -> Free e a -> Free f a
modifyEffects g (Pure a) = Pure a
modifyEffects g (Free e) = ...
```

- Complete the definition of `modifyEffects`.

- Using `modifyEffects`, implement `localEnv`.


#### Hints

Define `modiifyState` using `getState` and `putState`.

`modifyEffects` is a recursive function.


#### Solution 

<details>
<summary>Open this to see the answer</summary>

```Haskell
getState :: EvalM State
getState = Free $ StateGet $ pure

putState :: State -> EvalM ()
putState s = Free $ StatePut s $ pure ()

modifyState :: (State -> State) -> EvalM ()
modifyState f = do
  s <- getState
  putState $ f s
  
modifyEffects :: (Functor e, Functor f) => (e (Free e a) -> f (Free f a)) -> Free e a -> Free f a
modifyEffects g (Pure a) = Pure a
modifyEffects g (Free e) = Free $ modifyEffects g <$> g e

localEnv :: (Env -> Env) -> EvalM a -> EvalM a
localEnv f = modifyEffects g
  where
    g (ReadOp k) = ReadOp $ k . f
    g op = op
```

</details>
