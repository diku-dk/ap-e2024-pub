# Haskell programming hints

## Shadowing prelude functions

Sometimes you may want to define a variable or parameter with a nice
name like `init` or `tail`, only for GHC to yell at you that you are
shadowing some function you've never heard of:

```
This binding for ‘init’ shadows the existing binding
      imported from ‘Prelude’ at ...
      (and originally defined in ‘GHC.List’)
```

This is because functions with these names are exposed from the
`Prelude` module, which is implicitly imported into every Haskell
module. We can hide names from `Prelude` by *explicitly* importing it,
like any other module, and specify the names we *don't* want to
get:

```Haskell
import Prelude hiding (init)
```

## Warnings in `ghci`

By default, `ghci` (the Haskell REPL) will not print warnings about
your code. This is unfortunate, since many of the warnings are useful.
We can make `ghci` print warnings by using the `:set` command at the
`ghci` prompt:

```
λ> :set -Wall
```

Unfortunately, some warnings are quite noisy for interactive use. In
particular the one that warns about type defaults:

```
λ> 2+2

<interactive>:191:1: warning: [GHC-18042] [-Wtype-defaults]
    • Defaulting the type variable ‘a0’ to type ‘Integer’ in the following constraints
        (Show a0) arising from a use of ‘print’ at <interactive>:191:1-3
        (Num a0) arising from a use of ‘it’ at <interactive>:191:1-3
    • In a stmt of an interactive GHCi command: print it
4
```

This warning arises because the literal `2` is polymorphic (can have
any integral type), and barring any external constraints, `ghci`
defaults to assigning it the type `Integer`. This can be a useful
warning when writing application code, but is a bit annoying when
playing around in `ghci`. We can silence this specific warning as
follows:

```
λ> :set -Wno-type-defaults
```

## Accessing modules in `cabal repl`

Plain `ghci` works only when you use exclusively the standard library.
In other cases, we must use `cabal repl` to also load any dependencies
specified in the `.cabal` file. After running `cabal repl`, your
modules have been *loaded*, but their definitions are not available at
the REPL. A module `Foo.Bar` can be made available via the `:m`
command, as follows:

```
> :m *Foo.Bar
```

The asterisk makes unexported definitions available as well, which is
usually desirable when testing.

After modifying the module, use `:r` (without arguments) to reload any
changes.

## Useful modules

AP is not a course that requires much knowledge about [the Haskell
base library](https://hackage.haskell.org/package/base), but there are
some modules that contain utility functions you may find useful in
your work:

* [Control.Exception](https://hackage.haskell.org/package/base-4.20.0.1/docs/Control-Exception.html#v:catch)

* [Control.Monad](https://hackage.haskell.org/package/base-4.20.0.1/docs/Control-Monad.html)

* [Data.Char](https://hackage.haskell.org/package/base-4.20.0.1/docs/Data-Char.html)

* [Data.Either](https://hackage.haskell.org/package/base-4.20.0.1/docs/Data-Either.html)

* [Data.Foldable](https://hackage.haskell.org/package/base-4.20.0.1/docs/Data-Foldable.html)

* [Data.List](https://hackage.haskell.org/package/base-4.20.0.1/docs/Data-List.html)

* [Data.Maybe](https://hackage.haskell.org/package/base-4.20.0.1/docs/Data-Maybe.html)

* [Data.Ord](https://hackage.haskell.org/package/base-4.20.0.1/docs/Data-Ord.html)
