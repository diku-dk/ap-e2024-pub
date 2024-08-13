# Week 4 - Monadic Parsing

## Slides and Material

TBA

## Suggested Reading

* [Course Notes Chapter 3](https://diku-dk.github.io/ap-notes/chapter_3.html)
* [Grammars and parsing with Haskell using Parser Combinators](parsernotes.pdf)

## Exercises

In these exercises you will construct a parser for APL, using the same
AST definition as in the previous weeks. You are also given a [partial
interpreter](handout/apl.hs), corresponding to the solution to last
week's exercises, which allows you to actually run APL programs from
the command line. Feel free to replace this with the fully featured
`APL.Eval` module you developed for your assignments.

As always, the code handout is in [handout/](handout/) and a full
solution is in [solution](solution/).

### Getting started

The parser lives in the `APL.Parser` module you will put your tests in
`APL.Parser_Tests`. The parser module uses
[Megaparsec](https://hackage.haskell.org/package/megaparsec) as its
parser library. It contains various useful imports already, although
you are free to add more if you want.

We will start by implementing a parser for this very simple grammar,
written in
[EBNF](https://en.wikipedia.org/wiki/Extended_Backus%E2%80%93Naur_form):

```
digit = "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9";
int = {digit};
Exp ::= int "+" int;
```

In the grammar, we write terminals in all-lowercase or enclosed in
single or double quotes, and nonterminals are written with a leading
capital letter.

For the Haskell code we follow the convention discussed in the course
notes where *lexer functions* are named with a leading `l`, and are
the only functions that are allowed to directly manipulate whitespace.
Parser functions are named with a leading `l`, and must not directly
manipulate whitespace (such as by using the `spaces` parser).

Implement a lexer function

```Haskell
lInteger :: Parser Integer
```

that parses integers according to the grammar above (that is, one or
more decimal digits).

To start, let us not worry about whitespace or similar complexities.

#### Hints

You can use the function `read` to convert a string containing decimal
numbers to a Haskell `Integer`. Note that `read` will blow up your
program if the input is malformed, so be carefu.

Use `parseTest` to easily test your parser in the REPL:

```
> parseTest lInteger "123"
123
```

Use `isDigit` from `Data.Char` to determine whether a character
corresponds to a decimal digit.

Use the `some` combinator to repeatedly apply a parser and return a
list of the results.

Use the `satisfy` combinator to parse a single character satisfying
some predicate.

#### Solution

<details>
<summary>Open this to see the answer</summary>

```Haskell
lInteger :: Parser Integer
lInteger = read <$> some (satisfy isDigit)
```

</details>

### Handling whitespace

`lInteger` doese not properly handle whitespace, which can be seen by
trying to parse an integer followed by end-of-input (`eof`), when the
string has trailing whitespace:

```Haskell
> parseTest (lInteger <* eof) "123 "
1:4:
  |
1 | 123
  |    ^
unexpected space
expecting end of input
```

Remember (from the course material) that the way we handle whitespace
is for each function to consume *trailing* whitespace.

Define a combinator

```Haskell
lexeme :: Parser a -> Parser a
```

that applies a given parser, consumes trailing whitespace, then
returns the value of that parser.

<details>
<summary>Open this to see the answer</summary>

```Haskell
lexeme :: Parser a -> Parser a
lexeme p = p <* space
```

</details>

Now use `lexeme` to modify `lInteger` to consume trailing whitespace
such that the `parseTest` example above.

<details>
<summary>Open this to see the answer</summary>

```Haskell
lInteger :: Parser Integer
lInteger = lexeme $ read <$> some (satisfy isDigit)
```

</details>

### Correct tokenisation

The `lInteger` definition still has a subtle issue that will become a
problem later, but we might as well fix it immediately to establish
good habits. To illustrate the example, suppose that we also have a
parser for parsing identifiers like `xyz`. (We will write such a
parser in a bit, but for now it will live only in our imagination). If
then write a combined parser that parses *first* an integer, then a
identifier, then a string such as `123xyz` would be parsed into an
integer and a variable. This *may* be what we want, but it is not in
accordance with normal conventions regarding syntax, where tokens must
be separated from other tokens by either non-alphanumeric characters
or whitespace.

We want APL to follow normal conventions. That is, we want this
result:

```
> parseTest lInteger "123xyz"
1:4:
  |
1 | 123xyz
  |    ^
unexpected 'x'
```

Try it on your current definition and you will see a different result.

Modify your `lInteger` definition such that it only succeeds if *not*
followed by an alphabetic character.

#### Hints

Use `notFollowedBy` from Megaparsec.

#### Solution

<details>
<summary>Open this to see the answer</summary>

```Haskell
lInteger :: Parser Integer
lInteger =
  lexeme $ read <$> some (satisfy isDigit) <* notFollowedBy (satisfy isAlpha)
```

</details>

### Integer expressions and tests

Using `lInteger` as a building block, change `pExp` such that it can
parse integer expressions (using the `CstInt` constructor).

Then go to `APL.Parser_Tests` and add some tests that test whether you
can parse integers. Use the provided `parserTest` and `parserTestFail`
helper functions. Consider using nested `testGroup`s to organise the tests.

#### Solution

<details>
<summary>Open this to see the answer</summary>

In `APL.Parser`:

```Haskell
pExp :: Parser Exp
pExp = CstInt <$> lInteger
```

In `APL.Parser_Tests`:

```Haskell
parserTests :: TestTree
parserTests =
  testGroup
    "Parsing"
    [ testGroup
        "Constants"
        [ parserTest "123" $ CstInt 123,
          parserTest " 123" $ CstInt 123,
          parserTest "123 " $ CstInt 123,
          parserTestFail "123xyz"
        ]
    ]
```

</details>

### Parsing identifiers

An APL identifier consists of one or more alphanumeric characters.
Implement a parser

```Haskell
lVName :: Parser VName
```

that parses APL identifiers, including consuming trailing whitespace.

Also modify the `pExp` parser to handle identifiers (producing `Var`
nodes) and add appropriate tests.

#### Solution

<details>
<summary>Open this to see the answer</summary>

```Haskell
lVName :: Parser VName
lVName = lexeme $ some $ satisfy isAlpha

pExp :: Parser Exp
pExp =
  choice
    [ CstInt <$> lInteger,
      Var <$> lVName,
    ]
```

</details>

### Parsing Boolean values

TODO

#### Solution

<details>
<summary>Open this to see the answer</summary>

```Haskell
lBool :: Parser Bool
lBool =
  try $ lexeme $
    choice
      [ const True <$> lKeyword "true",
        const False <$> lKeyword "false"
      ]

pExp :: Parser Exp
pExp =
  choice
    [ CstInt <$> lInteger,
      CstBool <$> lBool,
      Var <$> lVName,
    ]
```

</details>

### Excluding keywords

In most programming languages, some names are *keywords* (sometimes
called *reserved words*) which are not allowed as identifiers. APL has
the following keywords: if, then, else, true, false, let, in, try,
catch, print, put, get. Modify `lVName` such that it fails if the
parsed name is a keyword.

#### Example

```
> parseTest lVName "if"
1:3:
  |
1 | if
  |   ^
Unexpected keyword
```

Note that the error location is a bit weird. This is because we check
whether the identifier is a keyword after it has been read, as far as
Megaparsec is concerned. Megaparsec does provide a facility for
providing the correct error location, but that is beyond the scope of
this course.

#### Hints

Use `fail` to signal a parse error if the identifier is a keyword.

Because we are signaling a parse error after reading input, you must
wrap the parser in the `try` combinator to indicate a backtracking
point. If you forget to do so, your code will seem to work for now,
but you will have subtle issues when we start to make use of keywords.

#### Solution

<details>
<summary>Open this to see the answer</summary>

```Haskell
lVName :: Parser VName
lVName = lexeme $ try $ do
  v <- some $ satisfy isAlphaNum
  if v `elem` reserved
    then fail "Unexpected keyword"
    else pure v
```

</details>

### Implement infix operators

In this task we will implement infix operators without taking operator
precedence into account. The challenge is handling left recursion.

### Running the interpreter

You have now implemented enough of the parser for the interpreter to
work for simple programs.

Use `cabal run apl PROG` to run the interpreter program `apl` on a
file `PROG`, which must contain a valid APL expression. The resulting
value (or an error message) will be printed to standard output and
standard error, respectively. You can also do `cabal install` to
install the `apl` executable in `$HOME/.local/bin`, after which you
can simply do `apl PROG` to run programs.

There is no task here. Congratulate yourself on having written a full
Haskell program and continue on.

### Implement `if-then-else`
