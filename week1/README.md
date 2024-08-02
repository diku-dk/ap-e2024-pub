# Week 1 - Programming in Haskell

In this week we will look at the basics of Haskell programming,
including type classes. While we do have to spend some time on the
minutiae of syntax and basic definitions, our true goal is to start on
acquiring an understanding of how to structure programs along abstract
and principled lines. Type classes are the most explicit example of
this idea.

## Slides and material

TBA

## Suggested Reading

* Part 1 of *Programming in Haskell*. Most of this is standard
  functional programming material, so feel free to skim those parts.
  The most interesting chapter is chapter 8.

## Exercises

The mandatory assignment builds directly on the concepts *and code*
developed during the exercises. You are strongly adviced to complete
the exercises before embarking on the assignment. It may also be a
useful exercise to do other exercises (unrelated to the assignment),
for example the ones in *Programming in Haskell*.

In these exercises you will be developing an interpreter for a very
simple language of arithmetic expressions, which we call *APL* for *AP
Language*.

Your starting point is found in [handout/](handout/) and complete
solutions are in [solutions/](solutions/).

The starting point is a skeletal Haskell program consisting of the
modules `APL` (in `src/APL.hs`) and `APL_Tests` (in
`src/APL_Tests.hs`), as well as `apl.cabal` with build instructions.
You should start by running `cabal test` to ensure that your Haskell
setup is functional. This compiles everything and runs the (currently
empty) test suite. When going through these exercises, run `cabal
test` frequently to see whether your code still works. It is also a
good idea to use `ghci` to interactively debug and test your work.

The `APL` module will contain the definition of APL. Initially it is
almost empty.

The `APL_Tests` module should import definitions from `APL` and tests
them. It also imports unit testing modules from `tasty`. Initially it
imports almost nothing, so you will have to extend the `import`
statements with the things you need.

The `tests.hs` file is a very simple program that runs the test suite.
You should not modify it.

### Getting started

We will represent expressions by a Haskell datatype `Exp`. It will
initially be tiny, and then we will extend it as we go along. In the
`APL` module, add the following definition:

```Haskell
data Exp
  = CstInt Integer
  deriving (Eq, Show)
```

The `CstInt` constructor represents an expression that is a constant
integer, like `1`. The `Eq` instance is so we can compare expressions
with `==`, and the `Show` is so we can print them to the screen
(critical for debugging).

The result of *evaluating* an expression is a *value*, which we
represent with the type `Val`. Add the following definition to `APL`:

```Haskell
data Val
  = ValInt Integer
  deriving (Eq, Show)
```

(These types are currently isomorphic, but will not remain so for
long.)

We will now define an evaluation function that can transform an
expression to a value. Define a function `eval` with the following
type:

```Haskell
eval :: Exp -> Val
```

Implement the `eval` function case for the `CstInt` case. A constant
integer evaluates (unsurprisingly) to the corresponding integer value.

<details>
<summary>Open this to see the answer</summary>

```Haskell
eval (CstInt x) = ValInt x
```

</details>

Add a test to `APL_Tests.hs` that tests whether your implementation of
`CstInt` is correct. You will need to extend the `import` statements
at the beginning of the module to include at least the following:

```Haskell
import APL (Exp (..), Val (..), eval)
import Test.Tasty.HUnit (testCase, (@?=))
```

Sabotage either your implementation of `eval` or the test itself such
that the test fails, and ensure that this is detected by `cabal test`.
Then fix it before continuing.

### Implementing arithmetic

Add the following new constructors to `Exp`:

```Haskell
data Exp
  = ...
  | Add Exp Exp
  | Sub Exp Exp
  | Mul Exp Exp
  | Div Exp Exp
  | Pow Exp Exp
```

These correspond to addition, subtraction, multiplication, division,
and exponentiation. Implement the corresponding cases in `eval` and
add appropriate tests to `APL_Tests`.
