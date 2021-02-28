module CabalDocspec.Cpp (
    cpphs,
) where

import Peura

import qualified Distribution.Simple.Build.Macros as C
import qualified Language.Preprocessor.Cpphs      as Cpphs

import CabalDocspec.Trace
import CabalDocspec.Warning

-- | C-preprocess file
cpphs
    :: TracerPeu r Tr
    -> Version              -- ^ this package version
    -> [PackageIdentifier]  -- ^ package identifiers, for @cabal_macros.h@
    -> [Path Absolute]      -- ^ includes
    -> [(String, String)]   -- ^ additional defines
    -> Path Absolute        -- ^ filepath
    -> String               -- ^ file contents
    -> Peu r String
cpphs tracer pkgVer pkgIds includes defines path input = withRunInIO $ \runInIO -> do
    let cpphsActions = Cpphs.CpphsActions
            { Cpphs.cpphsPutWarning = \msg -> runInIO (putWarning tracer WCpphs msg)
            , Cpphs.cpphsDie        = \msg -> runInIO (die tracer msg)
            }
    --putInfo tracer $ show defines
    liftIO $ Cpphs.runCpphs cpphsActions cpphsOpts path' input'
  where

    path' = toFilePath path
    input' = unlines
        [ "#line 1 \"" ++ Cpphs.cleanPath "cabal_macros.h" ++ "\""
        , C.generatePackageVersionMacros pkgVer pkgIds
        , "#line 1 \"" ++ Cpphs.cleanPath path' ++ "\""
        , input
        ]

    cpphsOpts = Cpphs.defaultCpphsOptions
        { Cpphs.boolopts = cpphsBoolOpts
        , Cpphs.defines  = defines
        , Cpphs.includes = map toFilePath includes
        }

cpphsBoolOpts :: Cpphs.BoolOptions
cpphsBoolOpts = Cpphs.defaultBoolOptions
    { Cpphs.hashline = False
    , Cpphs.warnings = True
    }
