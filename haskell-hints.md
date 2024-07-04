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
