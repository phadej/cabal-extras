{-# LANGUAGE DeriveGeneric #-}
module CabalBundler.NixSingle.Input (
    ZZ (..),
    ZDep (..),
    ) where

import Peura
import qualified Zinza

-- cabal repl cabal-bundler-internal 
-- :m *CabalBundler.NixSingle.Input
-- import Prelude (writeFile)
-- Zinza.parseAndCompileModuleIO _moduleConfig "data/single.nix" >>= writeFile "src/CabalBundler/NixSingle/Template.hs"

_moduleConfig :: Zinza.ModuleConfig ZZ
_moduleConfig = Zinza.simpleConfig "CabalBundler.NixSingle.Template"
    [ "CabalBundler.NixSingle.Input"
    ]

data ZZ = ZZ
    { zDerivationName :: String
    , zComponentName  :: String
    , zExecutableName :: String
    , zCdeps          :: [String]
    , zHsdeps         :: NonEmpty ZDep
    }
  deriving (Show, Generic)

instance Zinza.Zinza ZZ where
    toType    = Zinza.genericToTypeSFP
    toValue   = Zinza.genericToValueSFP
    fromValue = Zinza.genericFromValueSFP

data ZDep = ZDep
    { zdepName     :: String
    , zdepVersion  :: String
    , zdepSha256   :: String
    , zdepRevision :: String
    }
  deriving (Show, Generic)

instance Zinza.Zinza ZDep where
    toType    = Zinza.genericToTypeSFP
    toValue   = Zinza.genericToValueSFP
    fromValue = Zinza.genericFromValueSFP
