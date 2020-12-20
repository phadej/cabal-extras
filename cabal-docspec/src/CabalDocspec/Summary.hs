module CabalDocspec.Summary where

import Peura

-- | Summary of a test run.
data Summary = Summary
    { sExamples :: !Int  -- ^ total examples
    , sTried    :: !Int  -- ^ tried
    , sSuccess  :: !Int  -- ^ successful
    , sErrors   :: !Int  -- ^ errors
    , sFailures :: !Int  -- ^ property failure
    , sSkipped  :: !Int  -- ^ skipped
    }
  deriving (Eq, Show)

instance Semigroup Summary where
    Summary a b c d e f <> Summary a' b' c' d' e' f' =
        Summary (a + a') (b + b') (c + c') (d + d') (e + e') (f + f')

instance Monoid Summary where
    mempty = Summary 0 0 0 0 0 0
    mappend = (<>)
