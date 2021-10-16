module CabalHaddockServer.Warning where

import Peura

data W
    = WMissingHoogleFile
    | WMultipleHoogleFiles
    | WHoogle
  deriving (Eq, Ord, Enum, Bounded)

instance Universe W where universe = [minBound .. maxBound]
instance Finite W

instance Warning W where
    warningToFlag WMissingHoogleFile   = "missing-hoogle-file"
    warningToFlag WMultipleHoogleFiles = "multiple-hoogle-files"
    warningToFlag WHoogle              = "hoogle"
