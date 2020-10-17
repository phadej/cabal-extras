module CabalEnv.Warning where

import Peura

data W = WMissingCabalEnvData
  deriving (Eq, Ord, Enum, Bounded)

instance Universe W where universe = [minBound .. maxBound]
instance Finite W

instance Warning W where
    warningToFlag WMissingCabalEnvData = "missing-cabal-envdata"
