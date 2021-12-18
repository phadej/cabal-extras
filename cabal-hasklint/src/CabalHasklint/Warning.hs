module CabalHasklint.Warning where

import Peura

data W
    = WMultipleModuleFiles
    | WMissingModuleFile
    | WInvalidField
    | WCpphs
    | WUnqualImport
  deriving (Eq, Ord, Enum, Bounded)

instance Universe W where universe = [minBound .. maxBound]
instance Finite W

instance Warning W where
    warningToFlag WMultipleModuleFiles = "multiple-module-files"
    warningToFlag WMissingModuleFile   = "missing-module-file"
    warningToFlag WInvalidField        = "invalid-field"
    warningToFlag WCpphs               = "cpphs"

    warningToFlag WUnqualImport        = "unqual-import"
