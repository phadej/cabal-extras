module CabalDocspec.Warning where

import Peura

data W
    = WMultipleModuleFiles
    | WMissingModuleFile
    | WTimeout
    | WUnknownExtension
    | WInvalidField
    | WCpphs
    | WErrorInSetup
    | WSkippedProperty
  deriving (Eq, Ord, Enum, Bounded)

instance Universe W where universe = [minBound .. maxBound]
instance Finite W

instance Warning W where
    warningToFlag WMultipleModuleFiles = "multiple-module-files"
    warningToFlag WMissingModuleFile   = "missing-module-file"
    warningToFlag WTimeout             = "timeout"
    warningToFlag WUnknownExtension    = "unknown-extension"
    warningToFlag WInvalidField        = "invalid-field"
    warningToFlag WCpphs               = "cpphs"
    warningToFlag WErrorInSetup        = "error-in-setup"
    warningToFlag WSkippedProperty     = "skipped-property"
