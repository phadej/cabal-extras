module CabalDocspec.Warning where

import Peura

data W
    = WMultipleModuleFiles
    | WMissingModuleFile
    | WTimeout
    | WUnknownExtension
  deriving (Eq, Ord, Enum, Bounded)

instance Universe W where universe = [minBound .. maxBound]
instance Finite W

instance Warning W where
    warningToFlag WMultipleModuleFiles = "multiple-module-files"
    warningToFlag WMissingModuleFile   = "missing-module-file"
    warningToFlag WTimeout             = "timeout"
    warningToFlag WUnknownExtension    = "unknown-extension"
