{-# LANGUAGE CPP                 #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
-- |
-- Copyright: Oleg Grenrus
-- License: GPL-3.0-or-later
module CabalHasklint.Main (main) where

import Peura

import Control.Applicative ((<**>))
import Data.Monoid         (All (..))

import qualified Cabal.Config                                 as Cabal
import qualified Cabal.Plan                                   as Plan
import qualified Data.Map.Strict                              as Map
import qualified Distribution.Compiler                        as C
import qualified Distribution.ModuleName                      as C
import qualified Distribution.Package                         as C
import qualified Distribution.System                          as C
import qualified Distribution.Types.BuildInfo                 as C
import qualified Distribution.Types.CondTree                  as C
import qualified Distribution.Types.ConfVar                   as C
import qualified Distribution.Types.Flag                      as C
import qualified Distribution.Types.GenericPackageDescription as C
import qualified Distribution.Types.Library                   as C
import qualified Distribution.Utils.Path                      as C
import qualified Distribution.Version                         as C
import qualified Options.Applicative                          as O
import qualified System.FilePath                              as FP

import CabalHasklint.Lint
import CabalHasklint.Opts
import CabalHasklint.Package
import CabalHasklint.Parse
import CabalHasklint.Trace
import CabalHasklint.Warning

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

main :: IO ()
main = do
    opts <- O.execParser optsP'
    tracer0 <- makeTracerPeu (optTracer opts defaultTracerOptions)

    let dynOpts :: DynOpts
        dynOpts = optDynOpts opts defaultDynOpts

    -- modify tracer verbosity
    let tracer :: TracerPeu () Tr
        tracer = adjustTracer (optVerbosity dynOpts) tracer0

    -- main action
    runPeu tracer () $ do
        -- gather info
        ghcInfo <- getGhcInfo tracer (optCompiler opts)
        cabalCfg  <- liftIO Cabal.readConfig

        All res <- case optCabalPlan opts of
            CabalPlan -> do
                buildDir <- makeAbsolute (optBuildDir opts)
                plan <- liftIO $ Plan.findAndDecodePlanJson $ Plan.InBuildDir $ toFilePath buildDir

                -- checks
                checkGhcVersion tracer ghcInfo plan

                -- Elaborate plan by reading local package definitions
                pkgs0 <- readLocalCabalFiles tracer plan
                pkgs <-
                    if null (optTargets opts)
                    then return pkgs0
                    else fmap concat $ for (optTargets opts) $ \target -> do
                        let match =
                                [ pkg
                                | pkg <- pkgs0
                                , prettyShow (C.packageName (pkgGpd pkg)) == target
                                ]

                        case match of
                            []  -> die tracer $ "No package " ++ target ++ " in the plan"
                            _   -> return match

                -- collect environment of datadirs
                let env :: [(String, String)]
                    env = [] -- not used.

                -- process components
                res <- for pkgs $ \pkg -> do
                    for (pkgUnits pkg) $ \unit ->
                        ifor (Plan.uComps unit) $ \cn ci -> do
                            testComponent tracer0 tracer (optDynOpts opts)
                                ghcInfo buildDir cabalCfg plan env pkg unit cn ci

                -- summarize Alls
                return $ foldMap (foldMap (foldMap id)) res

            NoCabalPlan -> die tracer "No cabal plan mode is not yet implemneted" -- TODO

        unless res $ do
            putError tracer "there were some lint warnings"
            exitFailure
  where
    optsP' = O.info (optsP <**> O.helper <**> versionP) $ mconcat
        [ O.fullDesc
        , O.header "cabal-docspec - another doctest for Haskell"
        ]

    versionP = O.infoOption VERSION_cabal_hasklint
        $ O.long "version" <> O.help "Show version"

-------------------------------------------------------------------------------
-- Tracer adjustment
-------------------------------------------------------------------------------

adjustTracer :: Applicative m => Verbosity -> Tracer m (Trace Tr) -> Tracer m (Trace Tr)
adjustTracer (Verbosity verbosity) tracer0 = Tracer $ \cs t -> case t of
    TraceApp tr
        | verbosity >= trLevel tr
        -> traceWithCallStack tracer0 cs t

        | otherwise
        -> pure ()

    _ -> traceWithCallStack tracer0 cs t
  where
    trLevel TraceComponent {} = 0
    trLevel TraceParse {}     = 0
    trLevel TraceLint {}      = 0

-------------------------------------------------------------------------------
-- Checks
-------------------------------------------------------------------------------

checkGhcVersion :: TracerPeu r w -> GhcInfo -> Plan.PlanJson -> Peu r ()
checkGhcVersion tracer ghcInfo plan
    | ghcId == expected, planId == expected = return ()
    | otherwise = die tracer $ unwords
        [ ghcPath ghcInfo
        , "(" ++ prettyShow ghcId ++ ")"
        , "and plan compiler version"
        , "(" ++ prettyShow planId ++ ")"
        , "are not the expected"
        , prettyShow expected
        ]
  where
    ghcId    = PackageIdentifier "ghc" (ghcVersion ghcInfo)
    planId   = toCabal (Plan.pjCompilerId plan)
    expected = PackageIdentifier "ghc" (mkVersion [9,0,1])

-------------------------------------------------------------------------------
-- With plan.json
-------------------------------------------------------------------------------

testComponent
    :: TracerPeu r Tr
    -> TracerPeu r Tr
    -> (DynOpts -> DynOpts)
    -> GhcInfo
    -> Path Absolute -- ^ buildDir
    -> Cabal.Config Identity
    -> Plan.PlanJson
    -> [(String, String)]
    -> Package
    -> Plan.Unit
    -> Plan.CompName
    -> Plan.CompInfo
    -> Peu r All
testComponent tracer0 tracerTop dynOptsCli ghcInfo _buildDir _cabalCfg plan _env pkg unit cn@Plan.CompNameLib ci = do
    traceApp tracerTop $ TraceComponent (C.packageId (pkgGpd pkg)) cn

    -- "configure"
    lib0 <- maybe (die tracerTop "no library component in GPD") return
        $ C.condLibrary $ pkgGpd pkg
    let (_, lib) = simplifyCondTree ghcInfo (Map.mapKeys toCabal $ Plan.uFlags unit) lib0
    let bi = C.libBuildInfo lib

    -- append options in .cabal file
    dynOptsBi <- dynOptsFromBuildInfo tracerTop bi
    let dynOpts = dynOptsCli (dynOptsBi defaultDynOpts)

    -- Once dynOpts is read we can read adjust verbosity of our tracer
    let tracer = adjustTracer (optVerbosity dynOpts) tracer0

    -- find library module paths
    let compModules = C.exposedModules lib ++ C.otherModules bi
    modulePaths <- findModules
        tracer
        (pkgDir pkg)
        (C.hsSourceDirs bi)
        compModules 

    let pkgIds :: [PackageIdentifier]
        pkgIds =
            [ toCabal $ Plan.uPId unit'
            | unitId <- toList (Plan.ciLibDeps ci)
            , Just unit' <- return $ Plan.pjUnits plan ^? ix unitId
            ]

    -- cpp include dirs
    cppDirs <- traverse makeAbsolute (optCppIncludeDirs dynOpts)

    -- first phase: read modules and extract the comments
    let pkgVer = C.packageVersion (pkgGpd pkg)
    modules <- for modulePaths $ \(modname, modpath) ->
        parse tracer ghcInfo pkgVer (pkgDir pkg) cppDirs pkgIds bi modname modpath

    -- second phase: lint
    mconcat <$> traverse (lint tracer compModules bi) modules

-- Skip other components for now
testComponent _tracer0 _tracerTop _dynOpts _ghcInfo _buildDir _cabalCfg _plan _env _pkg _unit _cn _ci =
    return mempty

-------------------------------------------------------------------------------
-- Test component without
-------------------------------------------------------------------------------

-- TODO

-------------------------------------------------------------------------------
-- Utilities
-------------------------------------------------------------------------------

-- | Return name per module.
findModules
    :: TracerPeu r Tr
    -> Path Absolute                              -- ^ package root directory
    -> [C.SymbolicPath C.PackageDir C.SourceDir]  -- ^ @hs-source-dirs
    -> [C.ModuleName]
    -> Peu r [(C.ModuleName, Path Absolute)]
findModules tracer pkgRoot srcDirs' modules = do
    let srcDirs
            | null srcDirs' = [C.sameDirectory]
            | otherwise     = srcDirs'

    fmap concat $ for modules $ \modname -> do
        matches <- fmap concat $ for srcDirs $ \srcDir ->
            globDir1 (C.getSymbolicPath srcDir FP.</> C.toFilePath modname FP.-<.> ".hs") pkgRoot

        case matches of
            [m] -> return [(modname, m)]
            [] -> do
                putWarning tracer WMissingModuleFile $
                    "No .hs file for " ++ prettyShow modname
                return []
            (m:_) -> do
                putWarning tracer WMultipleModuleFiles $
                    "Multiple .hs files for " ++ prettyShow modname ++ ": " ++
                    unwords (map toFilePath matches)

                return [(modname, m)]

simplifyCondTree
    :: (Semigroup a, Semigroup d)
    => GhcInfo
    -> Map C.FlagName Bool
    -> C.CondTree C.ConfVar d a
    -> (d, a)
simplifyCondTree ghcInfo flags = C.simplifyCondTree $ \cv -> Right $ case cv of
    C.OS os         -> os == C.buildOS
    C.Arch arch     -> arch == C.buildArch
    C.Impl c vr     -> c == C.GHC && C.withinRange (ghcVersion ghcInfo) vr
    C.PackageFlag n -> Map.findWithDefault False n flags
