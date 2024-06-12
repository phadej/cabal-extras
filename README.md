# cabal-extras

A tool suite to aid Haskell development using `cabal-install`.
There are four tools in this repository:
- `cabal-env`: An experiment on what `cabal install --lib` could be.
- `cabal-diff`: Compare API of different package versions
- `cabal-bundler`: (ab)use `cabal-install` solver to build standalone installation bundles
- `cabal-deps`: An experiment on what `cabal outdated` could be.
- `cabal-store-check`: A naive tool to try to repair cabal's nix-store

All tools are highly experimental, although I (Oleg Grenrus) use them daily.

There's also [`cabal-fmt`](https://hackage.haskell.org/package/cabal-fmt), a `.cabal` file formatter, but it's more standalone tool.

# Installation

To install individual executables from this repository,

1. Make sure you have `GHC-8.2` and `cabal-install-3.0` or later installed.
2. Clone it with `git clone https://github.com/phadej/cabal-extras.git`
3. Install individual executables with `make install-env`, `make install-deps` etc.

You can pass flags to `cabal install` by setting `INSTALL_FLAGS`, e.g.

```
make INSTALL_FLAGS="--installdir $HOME/bin --install-method copy" install-env
```

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

## cabal-store-check

This is a small script which can find some broken packages in cabal nix-store.
It's a proof-of-concept of

- https://gitlab.haskell.org/ghc/ghc/merge_requests/2284
- https://github.com/haskell/cabal/issues/6060

### Synopsis

```
# Check if store package db is inconsistent
$ cabal-store-check
...
[   0.64132] error: haskell-ci-0.3.20190327-98543f1828739a9ad62f8722220f5d812f7f4a13f6fe2745a286c227593452b9 interface file for HaskellCI is missing
...

# You can remove broken packages with, which would repair the state
$ cabal-store-check --repair
```

## cabal-store-gc

This is another small script to reduce size of cabal's nix-store.

### Synopsis

```bash
# Add possible current projects dependencies as in direct root,
# and print reclaiming information
$ cabal-store-gc
...
...
[  16.89166] info: 262 components are referenced from the roots
[  16.89714] info: 183 components are in the store
[  16.89726] info: 393 components can be removed from the store
[  17.71338] info: 2328 MB can be freed

# If you want to perform the cleanup
$ cabal-store-gc --collect

# For more information, see
$ cabal-store-gc --help
```

### Roots

There are three kind of roots, which retain the packages in the store:

- executables in `installdir`. These are automatic roots.
- packages references from environments in `~/.ghc/.../environments/...`. These are also automatic roots.
- indirect roots, which are links from `~/.cabal/store/roots` to `plan.json`s
  elsewhere in the file system. Indirect links allow to retain development
  project dependencies.
  New indirect roots can be added with `--add-project-root` or `--add-root` actions.
