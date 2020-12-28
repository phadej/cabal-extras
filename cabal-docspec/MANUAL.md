NAME
====

cabal-docspec - another doctest for Haskell

SYNOPSIS
========

**cabal-docspec** *[OPTION]*... *[PACKAGE]...*  
**cabal-docspec** **\--no-cabal-plan** *[OPTION]*... *[CABALFILE]*...

DESCRIPTION
===========

*cabal-docspec* is a doctest runner closely integrated with *cabal-install*.
In common scenarious cabal-docspec is able to self-configure using cabal-install generated metadata (*plan.json*).
Another important difference is that cabal-docspec
doesn't depend on *ghc* library, but rather invoke the *ghci* executable.

cabal-docspec doesn't use GHC to parse input files,
but rather relies on *haskell-lexer* for comment extraction.
This approach is resilient, but not 100 per cent accurate.

cabal-docspec doesn't interpret library code in *ghci*, but instead loads precompiled code.
The effect is similar as using **-fobject-code** in GHCi (which is mandatory for packages with FFI, for example).
The consequence is that cabal-docspec is unable to evaluate doctest examples in non-exported modules (**other-modules**), or which use non-exposed symbols.

GHCi is invoked in a package directory, however it is told to not look for modules anywhere (with bare **-i** flag).
This way doctests may use package local files, but the code is not re-intepreted.

OPTIONS
=======

In  general, all boolean options are enabled with **--option** and yet again disabled with **\--no-option**.
That is, you use the exact same option name but prefix it with "no-".
However, in this list we mostly only list and show the --option version of them.

**-w, \--with-compiler** *path*

:   A path to compiler to use to run doctest examples. Must have the
    same version as in the cabal plan.

**\--ghc**

:   Indicate the used compiler is GHC. Currently this options is no-op.

**\--cabal-plan**
:   Read *plan.json* produced by cabal-install to find out project packages and their dependencies.
    When turned (with **\--no-cabal-plan**), paths to the cabal files have to be given.
    Also only the global package db is considered for dependencies.
    Default **\--cabal-plan**.

**\--preserve-it**

:   Preserve **it** variable, i.e. the result in of previous expression.
    Default **\--no-preserve-it**.

**\--strip-comments**

:   Strip Haskell comments from examples and the outputs.
    Especially outputs are assumed to be Haskell-like.
    Default **\--no-strip-comments**.

**\--setup** *expr*

:   An additional expression to execute as setup for all examples.
    Can be specified multiple times.

**\--extra-package** *pkgname*

:   An extra package to make available in GHCi session. The package must
    exist in the plan.

**\--timeout** *seconds*

:   Timeout for evaluation of single expression.
    Default: 3 seconds.
    Long timeouts may allow GHCi to acquire a lot of resources.
    On the other hand, too short timeout may cause false negatives.

**\-X** *extension*

:   Language extension to start GHCi session with.
    Can be specified multiple times.

**\--phase1**

:   Stop after the first phase.
    First phase consists of discovering the modules,
    and extracting the doctest examples from their comments.

**\--phase2**

:   Stop after the second phase, i.e. evaluation in GHCi phase.

**\--builddir** *dir*

:   Directory to look for **plan.json** and local package database.

**-v\, \--verbose**

:   Increase verbosity level.
    Can be specified multiple times.

**-q, \--quiet**

:   Decrease verbosity level.
    Can be specified multiple times.

**\--version**

:   Display numeric version.

**\--help**

:   Display short help message.

**\--man**

:   Display this manual.

CABAL FIELDS
============

It' is possible to provide cabal-docspec configuration
through fields in a .cabal file.

**\x-docspec-options:** *[OPTION]*...

:    These options will be applied *before* command line options,
     and allow configuration of cabal-docspec per component under test.

EXAMPLES
========

A simplest example, which should work for most packages is to run
**cabal-docspec** after **cabal v2-build all**:

    cabal v2-build all
    cabal-docspec

Testing base library inside GHC source tree.
The GHC source tree doesn't have cabal-install generated **plan.json**,
therefore we use **--no-cabal-plan** and supply the
*libraries/base/base.cabal* path.
There are some examples using explanatory comments,
**--strip-comments** makes them work.
Some examples are illustrating non-termination,
therefore short **--timeout** is justified.
Few examples are using symbols from *mtl* and *deepseq* packages,
we make them available.
Finally, some modules are documented with no-Prelude assumption,
therefore we have to turn it off.

    cabal-docspec -w $PWD/_build/stage1/bin/ghc \
        --no-cabal-plan \
        --strip-comments \
        --timeout 2 \
        --extra-package=mtl --extra-package=deepseq \
        -XNoImplicitPrelude \
        libraries/base/base.cabal

Third example is from *lens* library.
The *lens* library uses *simple-reflect* library for illustration of some examples.
However, *simple-reflect* is not a dependency of lens library.
One way to have add such dependency is to create dummy test-suite with it.

    test-suite doctests
        type:             exitcode-stdio-1.0
        main-is:          doctests.hs
        hs-source-dirs:   tests
        default-language: Haskell2010
        build-depends:    base, simple-reflect >= 0.3.1

Where **doctests.hs** doesn't need to do anything in particular, for example
it could be:

    module Main where

    main :: IO ()
    main = do
        putStrLn "This test-suite exists only to add dependencies"
        putStrLn "To run doctests: "
        putStrLn "    cabal build all --enable-tests"
        putStrLn "    cabal-docspec"

The bare **cabal-docspec** command works, because needed extra packages
are configured using **x-docspec-options** field in a package definition
library stanza:

    library
       ...

       x-docspec-options: --extra-package=simple-reflect

WARNINGS
========

All warnings are enabled by default.

**-Wmultiple-module-files**

:   Found multiple files matching the exposed module.

**-Wmissing-module-file**

:   No files found matching a module.
    For example modules which are preprocessed (*.hsc* etc).

**-Wtimeout**

:   Evaluation of an expression timed out.

**-Wunknown-extension**

:   Warn if extension passed via **-X** seems to be unknown.
    The known extension list is from *Cabal* library.

KNOWN BUGS AND INFECILITIES
===========================

Multiline input is not well supported for GHC prior 7.8, as these lack way to suppress secondary prompt output.

Properties (**prop>**) are recognized but not evaluated.

Literate Haskell is not supported.

C preprocessor (*cpphs*) warnings are not part of warning framework.

SEE ALSO
========

doctest(1) https://hackage.haskell.org/package/doctest

WWW (REPORTING BUGS)
====================

https://github.com/phadej/cabal-extras

COPYRIGHT
=========

Copyright © 2020-2021 Oleg Grenrus. License GPLv2-or-later: GNU GPL version 2 or later <http://gnu.org/licenses/gpl.html>.
This is free software: you are free to change and redistribute it. There is NO WARRANTY, to the extent permitted by law.

AUTHOR
======

Written by Oleg Grenrus.
Doctest comment extraction and comparison functions are originally from *doctest* by Simon Hengel.
