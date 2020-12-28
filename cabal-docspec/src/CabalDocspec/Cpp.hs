module CabalDocspec.Cpp (
    cpphs,
) where

import Peura

import qualified Distribution.Simple.Build.Macros as C
import qualified Language.Preprocessor.Cpphs      as Cpphs

import CabalDocspec.Trace

-- | C-preprocess file
cpphs
    :: TracerPeu r Tr
    -> [PackageIdentifier]  -- ^ package identifiers, for @cabal_macros.h@
    -> [(String, String)]   -- ^ additional defines
    -> Path Absolute        -- ^ filepath
    -> String               -- ^ file contents
    -> Peu r String
cpphs _tracer pkgIds defines path input = do
    --putInfo tracer $ show defines
    liftIO $ Cpphs.runCpphs cpphsOpts path' input'
  where
    path' = toFilePath path
    input' = unlines
        [ "#line 1 \"" ++ Cpphs.cleanPath "cabal_macros.h" ++ "\""
        , C.generatePackageVersionMacros pkgIds
        , "#line 1 \"" ++ Cpphs.cleanPath path' ++ "\""
        , input
        ]

    cpphsOpts = Cpphs.defaultCpphsOptions
        { Cpphs.boolopts = cpphsBoolOpts
        , Cpphs.defines  = defines
        -- includes --
        }

cpphsBoolOpts :: Cpphs.BoolOptions
cpphsBoolOpts = Cpphs.defaultBoolOptions
    { Cpphs.hashline = False
    , Cpphs.warnings = True -- TODO: change
    }
