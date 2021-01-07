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
In common scenarios cabal-docspec is able to self-configure using cabal-install generated metadata (*plan.json*).
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

In  general, all boolean options are enabled with **\--option** and yet again disabled with **\--no-option**.
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

**\--timeout-message** *message*

:   Message to return when the evaluation is timed out.
    Default is **\* Hangs forever \***.

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

**x-docspec-options:** *[OPTION]*...

:    These options will be applied *before* command line options,
     and allow configuration of cabal-docspec per component under test.

**x-docspec-extra-packages:** *[PKG]*...

:    A (space separated) list of extra packages. See **\--extra-package**.

EXAMPLES
========

A simplest example, which should work for most packages is to run
**cabal-docspec** after **cabal v2-build all**:

    cabal v2-build all
    cabal-docspec

Testing base library inside GHC source tree.
The GHC source tree doesn't have cabal-install generated **plan.json**,
therefore we use **\--no-cabal-plan** and supply the
*libraries/base/base.cabal* path.
There are some examples using explanatory comments,
**\--strip-comments** makes them work.
Some examples are illustrating non-termination,
therefore short **\--timeout** is justified.
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

The bare **cabal-docspec** command works, because needed extra packages are configured using **x-docspec-extra-packages** field in a package definition library stanza:

    library
       ...

       x-docspec-extra-packages: simple-reflect

WRITING DOCTESTS
================

**NOTE:** This section is edited version of a part of the *Doctest* README.markdown.
cabal-docspec reuses the way examples are specified.


Below is a small Haskell module.
The module contains a Haddock comment with some examples of interaction.
The examples demonstrate how the module is supposed to be used.

```haskell
module Fib where

-- | Compute Fibonacci numbers
--
-- Examples:
--
-- >>> fib 10
-- 55
--
-- >>> fib 5
-- 5
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)
```

A comment line starting with `>>>` denotes an _expression_.
All comment lines following an expression denote the _result_ of that expression.
Result is defined by what a REPL (e.g. ghci) prints to `stdout` and `stderr` when evaluating that expression.

Example groups
--------------

Examples from a single Haddock comment are grouped together and share the same
scope.  E.g. the following works:

```haskell
-- |
-- >>> let x = 23
-- >>> x + 42
-- 65
```

If an example fails, subsequent examples from the same group are skipped.  E.g.  for

```haskell
-- |
-- >>> let x = 23
-- >>> let n = x + y
-- >>> print n
```

`print n` is not tried, because `let n = x + y` fails (`y` is not in scope!).

A note on performance
---------------------

Because cabal-docspec uses compiled library, calling **:reload:** after each group doesn't cause performance problems.
For that reason, cabal-docspec doesn't have **\--fast** variant, it is not needed.

Setup code
----------

You can put setup code in a *named chunk* with the name **$setup**.
The setup code is run before each example group.
If the setup code produces any errors/failures, all tests from that module are skipped.

Here is an example:

```haskell
module Foo where

import Bar.Baz

-- $setup
-- >>> let x = 23 :: Int

-- |
-- >>> foo + x
-- 65
foo :: Int
foo = 42
```

Multi-line input
----------------

GHCi supports commands which span multiple lines, and the same syntax works for Doctest:

```haskell
-- |
-- >>> :{
--  let
--    x = 1
--    y = 2
--  in x + y + multiline
-- :}
-- 6
multiline = 3
```

Note that **>>>** can be left off for the lines following the first: this is so that haddock does not strip leading whitespace.
The expected output has whitespace stripped relative to the **:}**.

Some peculiarities on the ghci side mean that whitespace at the very start is lost.
This breaks the example *broken`* since the x and y aren't aligned from ghci's perspective.
A workaround is to avoid leading space, or add a newline such that the indentation does not matter:

```haskell
{- | >>> :{
let x = 1
    y = 2
  in x + y + works
:}
6
-}
works = 3

{- | >>> :{
 let x = 1
     y = 2
  in x + y + broken
:}
3
-}
broken = 3
```

Multi-line output
-----------------

If there are no blank lines in the output, multiple lines are handled automatically.

```haskell
-- | >>> putStr "Hello\nWorld!"
-- Hello
-- World!
```

If however the output contains blank lines, they must be noted explicitly with **<BLANKLINE>**.
For example,

```haskell
import Data.List ( intercalate )

-- | Double-space a paragraph.
--
--   Examples:
--
--   >>> let s1 = "\"Every one of whom?\""
--   >>> let s2 = "\"Every one of whom do you think?\""
--   >>> let s3 = "\"I haven't any idea.\""
--   >>> let paragraph = unlines [s1,s2,s3]
--   >>> putStrLn $ doubleSpace paragraph
--   "Every one of whom?"
--   <BLANKLINE>
--   "Every one of whom do you think?"
--   <BLANKLINE>
--   "I haven't any idea."
--
doubleSpace :: String -> String
doubleSpace = (intercalate "\n\n") . lines
```

Matching arbitrary output
-------------------------

Any lines containing only three dots (**...**) will match one or more lines with arbitrary content.
For instance,

```haskell
-- |
-- >>> putStrLn "foo\nbar\nbaz"
-- foo
-- ...
-- baz
```

If a line contains three dots and additional content, the three dots will match anything *within that line*:

```haskell
-- |
-- >>> putStrLn "foo bar baz"
-- foo ... baz
```

QuickCheck properties
---------------------

**NOTE:** cabal-docspec doesn't check properties at the moment. Details may change.

Haddock (since version 2.13.0) has markup support for properties
Doctest can verify properties with QuickCheck.
A simple property looks like this:

```haskell
-- |
-- prop> \xs -> sort xs == (sort . sort) (xs :: [Int])
```

The lambda abstraction is optional and can be omitted:

```haskell
-- |
-- prop> sort xs == (sort . sort) (xs :: [Int])
```

A complete example that uses setup code is below:

```haskell
module Fib where

-- $setup
-- >>> import Control.Applicative
-- >>> import Test.QuickCheck
-- >>> newtype Small = Small Int deriving Show
-- >>> instance Arbitrary Small where arbitrary = Small . (`mod` 10) <$> arbitrary

-- | Compute Fibonacci numbers
--
-- The following property holds:
--
-- prop> \(Small n) -> fib n == fib (n + 2) - fib (n + 1)
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)
```

If you see an error like the following, ensure that *QuickCheck* is a dependency of the test-suite or executable running docspec (to be corrected).

```haskell
<interactive>:39:3:
    Not in scope: ‘polyQuickCheck’
    In the splice: $(polyQuickCheck (mkName "doctest_prop"))

<interactive>:39:3:
    GHC stage restriction:
      ‘polyQuickCheck’ is used in a top-level splice or annotation,
      and must be imported, not defined locally
    In the expression: polyQuickCheck (mkName "doctest_prop")
    In the splice: $(polyQuickCheck (mkName "doctest_prop"))
```

Hiding examples from Haddock
----------------------------

You can put examples into named chunks, and not refer to them in the export list.
That way they will not be part of the generated Haddock documentation, but cabal-docspec will still find them.

```haskell
-- $
-- >>> 1 + 1
-- 2
```

Using GHC extensions
--------------------


There's two sets of GHC extensions involved when running Doctest:

1. The set of GHC extensions that are active when compiling the module code.
2. The set of GHC extensions that are active when executing the Doctest examples. (These are not influenced by the LANGUAGE pragmas in the file.)

Unlike Doctest, cabal-docspec doesn't compile libraries,
therefore you don't need to do anything special for the first point.

The recommended way to enable extensions for cabal-docspec examples is to specify them as **-X** flags.
Because set of enabled extensions persist even after **:reload**,
it is better to embrace that fact and enable them globally.

Another way to enable extensions, which is compatible with Doctest, is to switch them on like this:

```haskell
-- |
-- >>> :set -XTupleSections
-- >>> fst' $ (1,) 2
-- 1
fst' :: (a, b) -> a
fst' = fst
```

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

**-Winvalid-field**

:   Warn when parsing of cabal package file fields fails.

**-Wcpphs**

:   C preprocessor (*cpphs*) warnings.

KNOWN BUGS AND INFECILITIES
===========================

Properties (**prop>**) are recognized but not evaluated.

Literate Haskell is not supported.

Failures in the setup code does not cause module skip.

GHC-7.0 relies that *Char* type is in scope. This is an implementation artifact.

Q&A
====

Q: Why cabal-docspec doesn't import modules automatically?
-----------------------------------------------------

cabal-docspec tests library documentation from the outside.
It doesn't even try to look into an implementation for some secret bits, only to find examples.
In this sense it is more principled (than Doctest).
Therefore you might need to repeat imports in a **$setup** block.
OTOH, the implementation's imports never interfere with doctests.

Q: How to hide some Prelude imports, e.g. null?
----------------------------------------------

One way is to redefine the symbol in a **$setup** block using a qualified
module name.

```haskell
let null = Module.Under.Test.null
```

This way it will shadow both *Prelude.null* and *Module.Under.Test.null*,
and ambiguous symbol errors won't appear.

Another option is to use **-XNoImplicitPrelude** and import *Prelude*
explicitly.

Q: How cabal-docspec works with pre-8.0 version of GHC?
-------------------------------------------------------

cabal-docspec reads a *plan.json* file, which is generated by cabal-install.
That file contains (almost) all required information for cabal-docspec
to invoke ghci with the correct arguments.

Q: When does plan.json get generated?
-------------------------------------

It is generated by cabal-install as a side-effect of running the solver.
For example even

    cabal build --dry-run

is enough.
However, without libraries actually being built, cabal-docspec won't work.

Q: Does Doctest's --fast have an equivalent in cabal-docspec?
-------------------------------------------------------------

No, cabal-doctest doesn't need one.
The library code is loaded as pre-compiled object code, not interpreted code. As a result, the `:reload` command doesn't force code to be re-interpreted each time, making to cheap to run.
pre-compiled object, the **:reload** command is cheap.
It doesn't cause the re-interpretation of the sources.

Q: Are you envisioning making binary distributions of cabal-docspec available?
------------------------------------------------------------------------------

Yes.

Q: In the lens example, is test-suite somehow related to doctests?
------------------------------------------------------------------

No. The test-suite is there to ensure that the extra dependencies are built by cabal-install. 
We can also use a dummy package for that purpose,
but a test-suite is more lightweight.

As an alternative to this approach, with cabal-install-3.4 you may use 

    extra-packages: simple-reflect

in the *cabal.project* file.

Q: Are cabal build --disable-tests and cabal-docspec incompatible?
------------------------------------------------------------------

In general, no. As long as the library and extra dependencies used by doctests
are built, cabal-docspec shold work fine.

Q: What advantages cabal-docspec have over Doctest and .ghc.environment files?
------------------------------------------------------------------------------

There are a few differences.

1. The same cabal-docspec binary works with all GHC versions.
   Also with versions which don't have .ghc.environment file feature.

2. cabal-docspec doesn't interpret the source code.
   Though, Doctest could have that mode too.

3. Because cabal-docspec uses *plan.json* information,
   it doesn't have problems with the visibility of packages.
   For example *Prelude.Compat* from *base-compat* and *base-compat-batteries*
   won't cause ambiguous module problems, as long as the library being tested
   itself depends only on either one.

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
Doctest comment extraction and comparison functions are originally from *Doctest* by Simon Hengel.
*Cpphs* is written by Malcolm Wallace.
Other dependencies are written by their respective authors.
