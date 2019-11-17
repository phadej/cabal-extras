{-# LANGUAGE OverloadedStrings #-}
module CabalEnv.FakePackage (
fakePackage,
) where

import CabalEnv.Prelude

import qualified Data.Map.Strict            as Map
import qualified Distribution.Fields.Pretty as C
import qualified Distribution.Pretty        as C
import qualified Text.PrettyPrint           as PP

fakePackage :: Map PackageName VersionRange -> String
fakePackage deps = C.showFields (const [])
    [ fi "cabal-version" $ PP.text "2.4"
    , fi "name"          $ PP.text "fake-package"
    , fi "version"       $ PP.text "0"

    , C.PrettySection () "library" []
        [ fi "default-language" "Haskell2010"
        , fi "build-depends" $ PP.text "base"
        , fi "build-depends" $ PP.vcat
            [ PP.comma <+> C.pretty pn <+> C.pretty vr
            | (pn, vr) <- Map.toList deps
            ]
        ]
    ]
  where
    fi = C.PrettyField ()
