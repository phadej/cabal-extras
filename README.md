# cabal-extras

A tool suite to aid Haskell development using `cabal-install`.
There are four tools in this repository:
- `cabal-env`: An experiment on what `cabal install --lib` could be.
- `cabal-diff`: Compare API of different package versions
- `cabal-bundler`: (ab)use `cabal-install` solver to build standalone installation bundles
- `cabal-deps`: An experiment on what `cabal outdated` could be.

All tools are highly experimental, although I (Oleg Grenrus) use them daily.

There's also [`cabal-fmt`](https://hackage.haskell.org/package/cabal-fmt), a `.cabal` file formatter, but it's more standalone tool.

# Installation

To install individual executables from this repository,

1. Make sure you have `GHC-8.2` and `cabal-install-3.0` or later installed.
2. Clone it with `git clone https://github.com/phadej/cabal-extras.git`
3. Install individual executables with `make install-cabal-env`, `make install-cabal-deps` etc.

# Executables

## cabal-bundler

TBW

## cabal-diff

TBW

## cabal-deps

TBW

## cabal-env

### Synopsis

```
$ cabal-env optics
$ ghci
Prelude> import Optics
Prelude Optics>
```

TBW
