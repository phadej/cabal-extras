# cabal-core-inspection

[`inspection-testing`](https://hackage.haskell.org/package/inspection-testing)
was created over five years ago.
You may want to glance over Joachim Breitner [A promise checked is a promise kept: inspection testing](https://dl.acm.org/doi/10.1145/3242744.3242748)) Haskell Symposium paper
introducing it.

Already in 2018 I thought it's a fine tool, but it's more geared towards /library/ writers.
They can check on (some) examples that the promises they make about the libraries they write work at least on some examples.

What we cannot do with current `inspection-testing` is check that
the actual "real-life" use of the library works as intended.

Luckily, relatively recently, GHC got a feature to include *all Core bindings* in the interface files.
While [the original motivation](https://well-typed.com/blog/2023/02/interface-files-with-core/) is different (to make Template Haskell run fast),
the [-fwrite-if-simplified-core](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/phases.html#ghc-flag--fwrite-if-simplified-core)
enables us to inspect (as in inspection testing) the "production" Core (not the test examples).

This tool is very quick & dirty proof-of-concept of this idea.

Let me illustrate this with two examples.

In neither example I need to do any test setup, other than configuring
`cabal-core-inspection` (though configuration is now hardcoded).
Compare that to configuring e.g. HLint (HLint has user definable rules, and these are actually powerful tool).
In fact, `cabal-core-inspection` is nothing more than a linter for Core.

countChars
----------

First example is `countChars` as in Haskell Symposium Paper.
As far as I know, we cannot use `inspection-testing` in its current form to check anything about non-local bindings,
so if `countChars` is defined in an application, we would need to duplicate its definition to the test-suite to inspect it.
That is not great.

With Core inspection, we can look at the actual Core of the module (as it is in the compiler interface file).

The prototype doesn't have any configuration, but if we imagine it has we could ask it to check that `Example.countChars` should not contain type `Text`.
The prototype prints

```
Text value created with decodeUtf8With1 in countChars
```

So that's not the case. The intermediate `Text` value is created.
In fact, `text` doesn't promise that `toUpper` fuses with anything.

A nice thing about `cabal-core-inspection` that (in theory) it
could check *any definition in any module* as long as it's compiled with `-fwrite-if-simplified-core`.
So we could check things for our friends, if we care about something specific.

noGenerics
----------

Second example is related to GHC.Generics. I use a simple generic equality, but this could apply to any `GHC.Generics` based deriving.
(You should rather use `deriving stock Eq`, but generic equality is a simplest example which I remembered for now).

The generic equality might be defined in a library.
And library author may actually have tested it with `inspection-testing`.
But does it work on our type?

If we have

```haskell
data T where
    T1 :: Int -> Char -> T
    T2 :: Bool -> Double -> T
```

it does. The `cabal-core-inspection` doesn't complain.

But if we add a third constructor

```haskell
data T where
    T1 :: Int -> Char -> T
    T2 :: Bool -> Double -> T
    T3 :: ByteString -> T.Text -> T
```

`cabal-core-inspection` barfs:

```
Found L1 from GHC.Generics
Found :*: from GHC.Generics
Found R1 from GHC.Generics
```

The `T` becomes too large for GHC to want inline all the generics stuff.

It won't be fair to blame the library author, for example for

```haskell
data T where
    T1 :: Int -> T
    T2 :: Bool -> T
    T3 :: Char -> T
    T4 :: Double -> T
  deriving Generic
```

generic equality still optimises well, and doesn't have any traces of `GHC.Generics`.
We may actually need to (and may be adviced to) tune some GHC optimisation parameters.
But we need a way to check whether they are enough.
`inspection-testing` doesn't help, but a proper version of core inspection would be perfect for that task.

Conclusion
----------

The [-fwrite-if-simplified-core](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/phases.html#ghc-flag--fwrite-if-simplified-core) enables us to automate inspection of actual Core.
That is a huge win.
The `cabal-core-inspection` is just a proof-of-concept,
and I might try to make it into a real thing, but right now I don't have a real use case for it.

I'm also worried about `Note [Interface File with Core: Sharing RHSs]` in GHC. It says

> In order to avoid duplicating definitions for bindings which already have unfoldings we do some minor headstands to avoid serialising the RHS of a definition if it has *any* unfolding.
>
> * Only global things have unfoldings, because local things have had their unfoldings stripped.
> * For any global thing which has an unstable unfolding, we just use that.

Currently this optimisation is disabled, so `cabal-core-inspection` works,
but if it's enabled as is; then `INLINE`d bindings won't have their simplified
unfoldings preserved (but rather only "inline-RHS"), and that would destroy Core inspection possibility.

But until then, `cabal-core-inspection` idea works.
