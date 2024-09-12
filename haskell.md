# Guide for setting up a Haskell development environment

You will be using Haskell in Advanced Programming. You will not need a
fancy setup - all you need is a recent version of the Glasgow Haskell
Compiler (GHC), and the Cabal build tool. This document will tell you
how to install these. Other tools (such as editor integration) may be
useful to you, but is not required. We do not depend on very specific
versions, but assume that you are using at least the following
versions:

* GHC 9.6
* Cabal 3.10

If you follow this guide, you will install the right versions.

## If you use Windows

Install [WSL](https://learn.microsoft.com/en-us/windows/wsl/install)
and follow the instructions below. Some students have reported trouble
using `ghci` in the default terminal. Try using the new [Windows
Terminal](https://github.com/microsoft/terminal) instead.

## If you use Linux, macOS, or other reasonably common Unix variant

Use [ghcup](https://www.haskell.org/ghcup/) to install and manage the
Haskell compiler and tools. **Do not install GHC via the system
package manager or similar unless you know what you are doing and can
fix it yourself if it breaks.**

Follow the instructions in the link above. Make sure to also check the
[system
requirements](https://www.haskell.org/ghcup/install/#system-requirements),
as GHC requires a few system libraries to be installed, which is not
automatically done by `ghcup`.

When `ghcup` is installed, you can run `ghcup tui` to enter a menu
where you can install and "set" (activate) which version of GHC and
Cabal you want to use. We recommend GHC 9.6.6 and Cabal 3.10.2.1.
Newer versions will probably work, but these are what we use for
testing.

## If you use NixOS

Use this [shell.nix](shell.nix).
