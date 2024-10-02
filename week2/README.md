# Week 2 - Monads

This week we will begin studying monadic programming in Haskell. This
topic is foundational to the remainder of the course.

## Suggested Reading

* Philip Wadler's [The Essence of Functional Programming](essence-of-functional-programming.pdf).

* Chapter 12 in *Programming in Haskell*. Chapter 10 may also be of
  interest.

* [All About Monads](https://wiki.haskell.org/All_About_Monads)

* [Course Notes Chapter 2](https://diku-dk.github.io/ap-notes/chapter_2.html)

### Going Beyond

* [Finding Success in Haskell](https://leanpub.com/finding-success-in-haskell)

## Exercises

In these exercises you will construct an interpreter that is similar
in functionality to the one developed in week 1. However, this time we
will implement the interpreter via an evaluation monad that abstracts
away book-keeping of environments and failures. The second mandatory
assignment will ask you to extend this monadic interpreter.

Your starting point is found in [handout/](handout/) and complete
solutions are in [solutions/](solutions/). For some of the tasks,
partial or complete solutions are also included inline below. In terms
of modules, the structure is identical to week 1.

### Getting Started

The `APL.AST` and `APL.Eval` modules already contains various
definitions that should be familiar to you from week 1. `APL.Eval`
also contains some stub definitions for you to fill out.

To start with, we will define `EvalM` such that it tracks evaluation
failure:

1. Change the definition of `EvalM a` such that it can represent two
cases: either a value of type `a`, or an error of type `Error`.

2. Implement `Functor`, `Applicative`, and `Monad` instances for
   `EvalM`.

3. Implement `runEval`.

4. Implement a function `failure` that is used for signalling failure
in the `EvalM` monad:

```Haskell
failure :: String -> EvalM a
```

#### Hints

This is completely equivalent to the `Either` monad, but you should
*not* just say `type EvalM = Either`. It must still be a `newtype`.

When implementing the `>>=` method, you should start by checking
whether the left-hand side represents a failure, and if so, return
that failure.

#### Solution

<details>
<summary>Open this to see the answer</summary>

```Haskell
newtype EvalM a = EvalM (Either Error a)

instance Functor EvalM where
  fmap _ (EvalM (Left e))  = EvalM $ Left e
  fmap f (EvalM (Right x)) = EvalM $ Right $ f x

  -- Alternatively: fmap = liftM

instance Applicative EvalM where
  pure x = EvalM $ Right x
  EvalM (Left e)  <*> _               = EvalM (Left e)
  _               <*> EvalM (Left e)  = EvalM (Left e)
  EvalM (Right f) <*> EvalM (Right x) = EvalM (Right (f x))

  -- Alternatively: (<*>) = ap

instance Monad EvalM where
  EvalM x >>= f = EvalM $ case x of
    Left err -> Left err
    Right x' ->
      let EvalM y = f x'
       in y

failure :: String -> EvalM a
failure s = EvalM $ Left s

runEval :: EvalM a -> Either Error a
runEval (EvalM x) = x
```

</details>

### Monadic evaluation

Implement the `eval` function to make use of the `EvalM` monad. You
should *not* directly construct (or destruct) `EvalM` values in the
`eval` function; leave that to the `Monad` instance and the `failure`
function.

Skip `TryCatch` for now; we will implement that in the next task.

When you are done, you can use `runEval (eval [] e)` to evaluate an
expression `e`. Remember to add appropriate tests to `APL_Tests.hs`.

#### Hints

The approach is largely the same as in week 1, where we define a
function class for every language construct. However, to evaluate
subexpressions, we have to use monadic binding, e.g:

```Haskell
eval env (Add e1 e2) = eval env e1 >>= \x ->
                       eval env e2 >>= \y -> ...
```

Or with `do`-notation:

```Haskell
eval env (Add e1 e2) = do
  x <- eval e1 env
  y <- eval e2 env
  ...
```

Once you have made it work, you should still consider writing a helper
function to isolate the common behaviour for binary operators, similar
to week 1.

#### Solution (partial)

<details>
<summary>Open this to see the answer</summary>

```Haskell
eval :: Env -> Exp -> EvalM Val
eval _ (CstInt x) = pure $ ValInt x
eval _ (CstBool b) = pure $ ValBool b
eval env (Var v) = do
  case envLookup v env of
    Just x -> pure x
    Nothing -> failure $ "Unknown variable: " ++ v
eval env (Add e1 e2) = do
  x <- eval env e1
  y <- eval env e2
  case (x, y) of
    (ValInt x', ValInt y') -> pure $ ValInt $ x' + y'
    _ -> failure "Non-integer operand"
```

</details>

### Implementing `TryCatch`

Now we will implement `TryCatch`. Start by defining the function
`catch`:

```Haskell
catch :: EvalM a -> EvalM a -> EvalM a
```

An application `catch x y` should execute `x`. If `x` succeeds, then
its result is the result of `catch`. If `x` fails, then `catch` should
execute `y`, and the result of `y` is the result of `catch`. This
means `y` is only executed if `x` fails.

`catch` should be considered part of the implementation of `EvalM`,
and is allowed to directly construct and destruct `EvalM` values.

Once you have implemented `catch`, implement the `eval` case for
`TryCatch` (which is very simple).

#### Hints

`catch` will need to take apart both of its operands:

```Haskell
catch :: EvalM a -> EvalM a -> EvalM a
catch (EvalM m1) (EvalM m2) = EvalM $
  ...
```

#### Solution


<details>
<summary>Open this to see the answer</summary>

```Haskell
catch :: EvalM a -> EvalM a -> EvalM a
catch (EvalM m1) (EvalM m2) = EvalM $
  case m1 of
    Left _ -> m2
    Right x -> Right x

eval env (TryCatch e1 e2) =
  eval env e1 `catch` eval env e2
```

</details>

### Adding an environment to `EvalM`

Factoring error handling into `EvalM` itself has significantly reduced
the amount of boilerplate code in `eval`, but there is still one piece
left: the explicit passing of the environment when evaluating
subexpressions. In this task you will extend `EvalM` to maintain an
environment, similar to the `Reader` monad.

Start by changing the definition of `EvalM` such that it maintains
environment of type `Env`, similar to the `Reader` monad. You will
need to modify every bit of that directly uses the `EvalM`
constructor, but you should not need to modify `eval`. Then using this
new information, implement these two functions:

```Haskell
askEnv :: EvalM Env

localEnv :: (Env -> Env) -> EvalM a -> EvalM a
```

`askEnv` retrieves the current environment, and `localEnv` executes an
action in an environment that has been modified using the given
function.

When using `runEval` to run an `EvalM` action, the environment is
initially `envEmpty`.

Change the type of `eval` to be the following:

```Haskell
eval :: Exp -> EvalM Val
```

And update the implementation to make use of `askEnv` and `localEnv`
instead of passing around the environment explicitly.

#### Hints

To evaluate an expression `e` in an environment extended with a
mapping of `var` to `val`: `localEnv (envExtend var val) (eval e)`.

To evaluate an expression `e` in a completely different environment:
`localEnv (\_ -> env) (eval e)`.

#### Solution (partial)

<details>
<summary>Open this to see the answer</summary>

```Haskell
newtype EvalM a = EvalM (Env -> Either Error a)

instance Functor EvalM where
  fmap f (EvalM x) =
    EvalM $ \env -> case x env of
      Right v -> Right $ f v
      Left err -> Left err

instance Applicative EvalM where
  pure x = EvalM $ \_env -> Right x
  EvalM ef <*> EvalM ex = EvalM $ \env ->
    case (ef env, ex env) of
      (Left err, _) -> Left err
      (_, Left err) -> Left err
      (Right f, Right x) -> Right (f x)

instance Monad EvalM where
  EvalM x >>= f = EvalM $ \env ->
    case x env of
      Left err -> Left err
      Right x' ->
        let EvalM y = f x'
         in y env

askEnv :: EvalM Env
askEnv = EvalM $ \env -> Right env

localEnv :: (Env -> Env) -> EvalM a -> EvalM a
localEnv f (EvalM m) = EvalM $ \env -> m (f env)

failure :: String -> EvalM a
failure s = EvalM $ \_env -> Left s

catch :: EvalM a -> EvalM a -> EvalM a
catch (EvalM m1) (EvalM m2) = EvalM $ \env ->
  case m1 env of
    Left _ -> m2 env
    Right x -> Right x

runEval :: EvalM a -> Either Error a
runEval (EvalM m) = m envEmpty

eval :: Exp -> EvalM Val
eval (CstInt x) = pure $ ValInt x
eval (CstBool b) = pure $ ValBool b
eval (Var v) = do
  env <- askEnv
  case envLookup v env of
    Just x -> pure x
    Nothing -> failure $ "Unknown variable: " ++ v
eval (Add e1 e2) = do
  x <- eval e1
  y <- eval e2
  case (x, y) of
    (ValInt x', ValInt y') -> pure $ ValInt $ x' + y'
    _ -> failure "Non-integer operand"
eval (Let var e1 e2) = do
  v1 <- eval e1
  localEnv (envExtend var v1) $ eval e2
eval (Lambda var body) = do
  env <- askEnv
  pure $ ValFun env var body
eval (Apply e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  case (v1, v2) of
    (ValFun f_env var body, arg) ->
      localEnv (const $ envExtend var arg f_env) $ eval body
    (_, _) ->
      failure "Cannot apply non-function"
```

</details>
