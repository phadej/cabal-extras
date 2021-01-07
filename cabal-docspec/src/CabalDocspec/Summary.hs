module CabalDocspec.Summary where

import Peura

import CabalDocspec.Doctest.Parse

-- | Summary of a test run.
data Summary = Summary
    { sSetup      :: !SubSummary
    , sExamples   :: !SubSummary
    , sProperties :: !SubSummary
    }
  deriving (Eq, Show)

sumSummary :: Summary -> SubSummary
sumSummary (Summary a b c) = a <> b <> c

instance Semigroup Summary where
    Summary a b c <> Summary a' b' c' =
        Summary (a <> a') (b <> b') (c <> c')

instance Monoid Summary where
    mempty = Summary mempty mempty mempty
    mappend = (<>)

data SubSummary = SubSummary
    { _ssTotal    :: !Int  -- ^ total cases
    , _ssTried    :: !Int  -- ^ tried
    , _ssSuccess  :: !Int  -- ^ successful
    , _ssErrors   :: !Int  -- ^ errors
    , _ssFailures :: !Int  -- ^ property failure
    , _ssSkipped  :: !Int  -- ^ skipped
    }
  deriving (Eq, Show)

instance Semigroup SubSummary where
    SubSummary a b c d e f <> SubSummary a' b' c' d' e' f' =
        SubSummary (a + a') (b + b') (c + c') (d + d') (e + e') (f + f')

instance Monoid SubSummary where
    mempty = SubSummary 0 0 0 0 0 0
    mappend = (<>)

isOk :: SubSummary -> Bool
isOk s = _ssErrors s == 0 && _ssFailures s == 0

ssSuccess :: SubSummary
ssSuccess = SubSummary 1 1 1 0 0 0

ssError :: SubSummary
ssError = SubSummary 1 1 0 1 0 0

ssFailure :: SubSummary
ssFailure = SubSummary 1 1 0 1 0 0

ssSkip :: SubSummary
ssSkip = SubSummary 1 0 0 0 0 1

skipDocTest :: DocTest -> Summary
skipDocTest Example {}  = mempty { sExamples   = ssSkip }
skipDocTest Property {} = mempty { sProperties = ssSkip }

-- In setup properties are always a failure.
skipSetupDocTest :: DocTest -> SubSummary
skipSetupDocTest Example {}  = ssSkip
skipSetupDocTest Property {} = ssFailure
