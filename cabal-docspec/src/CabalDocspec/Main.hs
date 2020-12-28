{-# LANGUAGE CPP                 #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
-- |
-- Copyright: Oleg Grenrus
-- License: GPL-3.0-or-later
module CabalDocspec.Main (main) where

import Peura

import Control.Applicative ((<**>))

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
import qualified Language.Haskell.Extension as Ext
import qualified Distribution.Types.GenericPackageDescription as C
import qualified Distribution.Types.InstalledPackageInfo      as IPI
import qualified Distribution.Types.Library                   as C
import qualified Distribution.Types.LibraryName               as C
import qualified Distribution.Version                         as C
import qualified Options.Applicative                          as O
import qualified System.FilePath                              as FP

import CabalDocspec.Doctest.Extract
import CabalDocspec.Doctest.Parse
import CabalDocspec.Lexer
import CabalDocspec.Located
import CabalDocspec.Man
import CabalDocspec.Opts
import CabalDocspec.Package
import CabalDocspec.Phase1
import CabalDocspec.Phase2
import CabalDocspec.Summary
import CabalDocspec.Trace
import CabalDocspec.Warning

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

main :: IO ()
main = do
    opts <- O.execParser optsP'
    tracer0 <- makeTracerPeu (optTracer opts defaultTracerOptions)

    -- modify tracer verbosity
    let tracer :: TracerPeu () Tr
        tracer = Tracer $ \cs t -> case t of
            TraceApp tr -> case optVerbosity opts of
                Quiet   -> case tr of
                    TraceSummary {} -> traceWithCallStack tracer0 cs t
                    _               -> pure ()
                Normal  -> case tr of
                    TracePhase1 {}  -> traceWithCallStack tracer0 cs t
                    TracePhase2 {}  -> traceWithCallStack tracer0 cs t
                    TraceSummary {} -> traceWithCallStack tracer0 cs t
                    _               -> pure ()
                Verbose -> traceWithCallStack tracer0 cs t
            _ -> traceWithCallStack tracer0 cs t

    runPeu tracer () $ do
        -- display manual
        when (optPhase opts == Manual) $ liftIO man

        -- check if we know these extensions
        for_ (optExts (optGhci opts)) $ \ext -> case Ext.classifyExtension ext of
            Ext.UnknownExtension _ -> putWarning tracer WUnknownExtension $
                ext ++ " is unknown extension. GHCi may fail to start."
            _ -> pure ()

        -- gather info
        ghcInfo <- getGhcInfo tracer (optCompiler opts)
        cabalCfg  <- liftIO Cabal.readConfig

        res <- case optCabalPlan opts of
            CabalPlan -> do
                unless (null (optTargets opts)) $
                    die tracer "Targets not supported for cabal.plan variant yet"

                builddir <- makeAbsolute (optBuilddir opts)
                plan <- liftIO $ Plan.findAndDecodePlanJson $ Plan.InBuildDir $ toFilePath builddir

                -- checks
                checkGhcVersion tracer ghcInfo plan

                -- Elaborate plan by reading local package definitions
                pkgs <- readLocalCabalFiles tracer plan

                -- process components
                res <- for pkgs $ \pkg -> do
                    for (pkgUnits pkg) $ \unit ->
                        ifor (Plan.uComps unit) $ \cn ci -> do
                            testComponent tracer (optPhase opts) (optStripComs opts) (optGhci opts) (optExtraPkgs opts)
                                ghcInfo builddir cabalCfg plan pkg unit cn ci

                -- summarize Summary's
                return $ foldMap (foldMap (foldMap id)) res

            NoCabalPlan -> do
                dbG <- readPackageDb tracer (ghcGlobalDb ghcInfo)
                pkgs <- readDirectCabalFiles tracer (optTargets opts)

                -- process components
                res <- for pkgs $ \pkg -> do
                    testComponentNo tracer (optPhase opts) (optStripComs opts) (optGhci opts)  (optExtraPkgs opts)
                        ghcInfo cabalCfg dbG pkg

                return $ foldMap id res

        -- if there are errors, exit
        traceApp tracer $ TraceSummary res
        unless (sErrors res == 0) $ do
            putError tracer "there were errors or property failures"
            exitFailure
  where
    optsP' = O.info (optsP <**> O.helper <**> versionP) $ mconcat
        [ O.fullDesc
        , O.header "cabal-docspec - another doctest for Haskell"
        ]

    versionP = O.infoOption VERSION_cabal_docspec
        $ O.long "version" <> O.help "Show version"

-------------------------------------------------------------------------------
-- Checks
-------------------------------------------------------------------------------

checkGhcVersion :: TracerPeu r w -> GhcInfo -> Plan.PlanJson -> Peu r ()
checkGhcVersion tracer ghcInfo plan
    | ghcId == planId = return ()
    | otherwise = die tracer $ unwords
        [ ghcPath ghcInfo
        , "(" ++ prettyShow ghcId ++ ")"
        , "and plan compiler version"
        , "(" ++ prettyShow planId ++ ")"
        , "are not the same"
        ]
  where
    ghcId = PackageIdentifier "ghc" (ghcVersion ghcInfo)
    planId = toCabal (Plan.pjCompilerId plan)

-------------------------------------------------------------------------------
-- With plan.json
-------------------------------------------------------------------------------

testComponent
    :: TracerPeu r Tr
    -> Phase
    -> StripComments
    -> GhciOpts
    -> [PackageName]
    -> GhcInfo
    -> Path Absolute -- ^ builddir
    -> Cabal.Config Identity
    -> Plan.PlanJson
    -> Package
    -> Plan.Unit
    -> Plan.CompName
    -> Plan.CompInfo
    -> Peu r Summary
testComponent tracer phase stripComments ghciOpts extraPkgs ghcInfo buildDir cabalCfg plan pkg unit cn@Plan.CompNameLib ci = do
    traceApp tracer $ TraceComponent (C.packageId (pkgGpd pkg)) cn

    -- "configure"
    lib0 <- maybe (die tracer "no library component in GPD") return
        $ C.condLibrary $ pkgGpd pkg
    let (_, lib) = simplifyCondTree ghcInfo (Map.mapKeys toCabal $ Plan.uFlags unit) lib0
    let bi = C.libBuildInfo lib

    -- find extra units
    extraUnitIds <- findExtraPackages tracer plan extraPkgs

    -- find library module paths
    modulePaths <- findModules
        tracer
        (pkgPath pkg)
        (C.hsSourceDirs bi)
        (C.exposedModules lib)

    let pkgIds :: [PackageIdentifier]
        pkgIds =
            [ toCabal $ Plan.uPId unit'
            | unitId <- toList (Plan.ciLibDeps ci)
            , Just unit' <- return $ Plan.pjUnits plan ^? ix unitId
            ]

    let unitIds :: [UnitId]
        unitIds = ordNub $
            toCabal (Plan.uId unit) :
            map toCabal (toList (Plan.ciLibDeps ci)) ++
            extraUnitIds

    -- first phase: read modules and extract the comments
    modules <- for modulePaths $ \(modname, modpath) ->
        phase1 tracer ghcInfo pkgIds bi modname modpath

    -- extract doctests from the modules.
    let parsed :: [Module [Located DocTest]]
        parsed = fmap4 (doctestStripComments stripComments) $ parseModules modules where

    if phase > Phase1
    then do
        phase2 tracer phase ghciOpts unitIds ghcInfo (Just buildDir) cabalCfg (pkgPath pkg) parsed
    else
        return $ foldMap (foldMap (\xs -> Summary (length xs) 0 0 0 0 (length xs)) . moduleContent) parsed

-- Skip other components
testComponent _tracer _phase _stripComments _ghciOpts _extraPkgs _ghcInfo _builddir _cabalCfg _plan _pkg _unit _cn _ci =
    return mempty

-------------------------------------------------------------------------------
-- Test component without
-------------------------------------------------------------------------------

testComponentNo
    :: forall r. TracerPeu r Tr
    -> Phase
    -> StripComments
    -> GhciOpts
    -> [PackageName]
    -> GhcInfo
    -> Cabal.Config Identity
    -> Map UnitId IPI.InstalledPackageInfo
    -> Package
    -> Peu r Summary
testComponentNo tracer phase stripComments ghciOpts extraPkgs ghcInfo cabalCfg dbG pkg = do
    traceApp tracer $ TraceComponent (C.packageId (pkgGpd pkg)) Plan.CompNameLib

    -- use default values for flags
    let flags :: Map C.FlagName Bool
        flags = Map.fromList
            [ (C.flagName flag, C.flagDefault flag)
            | flag <- C.genPackageFlags (pkgGpd pkg)
            ]

    -- "configure"
    lib0 <- maybe (die tracer "no library component in GPD") return
        $ C.condLibrary $ pkgGpd pkg
    let (_, lib) = simplifyCondTree ghcInfo flags lib0
    let bi = C.libBuildInfo lib

    let findUnit :: PackageName -> Peu r (UnitId, PackageIdentifier)
        findUnit pn = do
            let units =
                    [ (unitId, IPI.sourcePackageId ipi)
                    | (unitId, ipi) <- itoList dbG
                    , C.packageName (IPI.sourcePackageId ipi) == pn
                    , IPI.sourceLibName ipi == C.LMainLibName
                    ]

            case units of
                [u] -> return u
                []  -> die tracer $ "Cannot find unit for " ++ prettyShow pn
                _   -> die tracer $ "Found multiple units for " ++ prettyShow pn ++ ": " ++
                    unwords (map (prettyShow . fst) units)

    -- we don't have install plan, so we look for packages in IPI
    depends <- for (C.targetBuildDepends bi) $ \dep -> findUnit (C.depPkgName dep)
    thisUnitId <- findUnit (C.packageName (pkgGpd pkg))
    extraUnitIds <- traverse findUnit extraPkgs

    let pkgIds :: [PackageIdentifier]
        pkgIds = map snd depends

    let unitIds :: [UnitId]
        unitIds = ordNub $ map fst $
            thisUnitId : depends ++ extraUnitIds

    -- find library module paths
    modulePaths <- findModules
        tracer
        (pkgPath pkg)
        (C.hsSourceDirs bi)
        (C.exposedModules lib)

    -- first phase: read modules and extract the comments
    modules <- for modulePaths $ \(modname, modpath) ->
        phase1 tracer ghcInfo pkgIds bi modname modpath

    -- extract doctests from the modules.
    let parsed :: [Module [Located DocTest]]
        parsed = fmap4 (doctestStripComments stripComments) $ parseModules modules where

    if phase > Phase1
    then do
        -- tmpDir <- getTemporaryDirectory -- TODO: make this configurable
        phase2 tracer phase ghciOpts unitIds ghcInfo Nothing cabalCfg (pkgPath pkg) parsed
    else
        return $ foldMap (foldMap (\xs -> Summary (length xs) 0 0 0 0 (length xs)) . moduleContent) parsed

-------------------------------------------------------------------------------
-- Extra packages
-------------------------------------------------------------------------------

findExtraPackages
    :: TracerPeu r Tr
    -> Plan.PlanJson
    -> [PackageName]
    -> Peu r [UnitId]
findExtraPackages tracer plan = traverse $ \pn -> do
    let units =
          [ toCabal uid
          | (uid, unit) <- itoList (Plan.pjUnits plan)
          , let PackageIdentifier pn' _ = toCabal (Plan.uPId unit)
          , pn == pn'
          , Plan.CompNameLib `Map.member` Plan.uComps unit
          ]

    case units of
        [u] -> return u
        []  -> die tracer $ "Cannot find unit for " ++ prettyShow pn
        _   -> die tracer $ "Found multiple units for " ++ prettyShow pn ++ ": " ++
            unwords (map prettyShow units)

-------------------------------------------------------------------------------
-- Utilities
-------------------------------------------------------------------------------

-- | Return name per module.
findModules
    :: TracerPeu r Tr
    -> Path Absolute           -- ^ package root directory
    -> [String]                -- ^ @hs-source-dirs
    -> [C.ModuleName]
    -> Peu r [(C.ModuleName, Path Absolute)]
findModules tracer pkgRoot srcDirs' modules = do
    let srcDirs
            | null srcDirs' = ["."]
            | otherwise     = srcDirs'

    fmap concat $ for modules $ \modname -> do
        matches <- fmap concat $ for srcDirs $ \srcDir ->
            globDir1 (srcDir FP.</> C.toFilePath modname FP.-<.> ".hs") pkgRoot

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
    C.OS os     -> os == C.buildOS
    C.Arch arch -> arch == C.buildArch
    C.Impl c vr -> c == C.GHC && C.withinRange (ghcVersion ghcInfo) vr
    C.Flag n    -> Map.findWithDefault False n flags

-------------------------------------------------------------------------------
--
-------------------------------------------------------------------------------

doctestStripComments :: StripComments -> DocTest -> DocTest
doctestStripComments StripComments    (Example expr res) = Example (dropComments expr) (map f res) where
    f (ExpectedLine xs) = ExpectedLine (fmap g xs)
    f WildCardLine      = WildCardLine
    g (LineChunk s)     = LineChunk (dropComments s)
    g WildCardChunk     = WildCardChunk
doctestStripComments StripComments     (Property expr) = Property (dropComments expr)
doctestStripComments DontStripComments dt = dt

fmap4
    :: (Functor f1, Functor f2, Functor f3, Functor f4)
    => (a -> b) -> f1 (f2 (f3 (f4 a))) -> f1 (f2 (f3 (f4 b)))
fmap4 f = fmap (fmap (fmap (fmap f)))
