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
import qualified Data.Set                                     as Set
import qualified Distribution.Compiler                        as C
import qualified Distribution.ModuleName                      as C
import qualified Distribution.Package                         as C
import qualified Distribution.System                          as C
import qualified Distribution.Types.BuildInfo                 as C
import qualified Distribution.Types.CondTree                  as C
import qualified Distribution.Types.ConfVar                   as C
import qualified Distribution.Types.Flag                      as C
import qualified Distribution.Types.GenericPackageDescription as C
import qualified Distribution.Types.InstalledPackageInfo      as IPI
import qualified Distribution.Types.Library                   as C
import qualified Distribution.Types.LibraryName               as C
import qualified Distribution.Types.PackageDescription        as C
import qualified Distribution.Version                         as C
import qualified Language.Haskell.Extension                   as Ext
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

    let dynOpts :: DynOpts
        dynOpts = optGhci opts defaultDynOpts

    -- modify tracer verbosity
    let tracer :: TracerPeu () Tr
        tracer = adjustTracer (optVerbosity dynOpts) tracer0

    -- main action
    runPeu tracer () $ do
        -- display manual
        when (optPhase dynOpts == Manual) $ liftIO man

        -- check if we know these extensions
        for_ (optExts (optGhci opts defaultDynOpts)) $ \ext -> case Ext.classifyExtension ext of
            Ext.UnknownExtension _ -> putWarning tracer WUnknownExtension $
                ext ++ " is unknown extension. GHCi may fail to start."
            _ -> pure ()

        -- gather info
        ghcInfo <- getGhcInfo tracer (optCompiler opts)
        cabalCfg  <- liftIO Cabal.readConfig

        res <- case optCabalPlan opts of
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
                    env =
                        [ (manglePackageName pn ++ "_datadir", toFilePath $ pkgDir pkg </> fromUnrootedFilePath (C.dataDir pd))
                        | pkg <- pkgs0
                        , let pd = C.packageDescription (pkgGpd pkg)
                        , let pn = C.packageName pd
                        , not (null (C.dataFiles pd))
                        ]

                -- process components
                res <- for pkgs $ \pkg -> do
                    for (pkgUnits pkg) $ \unit ->
                        ifor (Plan.uComps unit) $ \cn ci -> do
                            testComponent tracer0 tracer (optGhci opts)
                                ghcInfo buildDir cabalCfg plan env pkg unit cn ci

                -- summarize Summary's
                return $ foldMap (foldMap (foldMap id)) res

            NoCabalPlan -> do
                dbG <- readPackageDb tracer (ghcGlobalDb ghcInfo)
                pkgs <- readDirectCabalFiles tracer (optTargets opts)

                -- process components
                res <- for pkgs $ \pkg -> do
                    testComponentNo tracer0 tracer (optGhci opts)
                        ghcInfo cabalCfg dbG pkg

                return $ foldMap id res

        -- if there are errors, exit
        traceApp tracer $ TraceSummary res
        let res' = sumSummary res
        unless (isOk res') $ do
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
-- Tracer adjustment
-------------------------------------------------------------------------------

adjustTracer :: Applicative m => Verbosity -> Tracer m (Trace Tr) -> Tracer m (Trace Tr)
adjustTracer (Verbosity verbosity) tracer0 = Tracer $ \cs t -> case t of
    TraceApp tr
        | verbosity >= trLevel tr
        ->traceWithCallStack tracer0 cs t

        | otherwise
        -> pure ()

    _ -> traceWithCallStack tracer0 cs t
  where
    trLevel TraceSummary {}   = minBound
    trLevel TraceComponent {} = 0
    trLevel TracePhase1 {}    = 0
    trLevel TracePhase2 {}    = 0
    trLevel TraceGHCi {}      = 1
    trLevel TraceGHCiInput {} = 1

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
    -> Peu r Summary
testComponent tracer0 tracerTop dynOptsCli ghcInfo buildDir cabalCfg plan env pkg unit cn@Plan.CompNameLib ci = do
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

    -- find extra units
    extraUnitIds <- findExtraPackages tracer plan $ Set.toList $ propPkgs dynOpts <> optExtraPkgs dynOpts

    -- find library module paths
    modulePaths <- findModules
        tracer
        (pkgDir pkg)
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

    -- cpp include dirs
    cppDirs <- traverse makeAbsolute (optCppIncludeDirs dynOpts)

    -- first phase: read modules and extract the comments
    modules <- for modulePaths $ \(modname, modpath) ->
        phase1 tracer ghcInfo (pkgDir pkg) cppDirs pkgIds bi modname modpath

    -- extract doctests from the modules.
    let parsed :: [Module [Located DocTest]]
        parsed = fmap4 (doctestStripComments (optStripComs dynOpts))
            $ parseModules modules where

    if optPhase dynOpts > Phase1
    then do
        phase2 tracer dynOpts unitIds ghcInfo (Just buildDir) cabalCfg (pkgDir pkg) env parsed
    else
        return $ foldMap skipModule parsed

-- Skip other components
testComponent _tracer0 _tracerTop _dynOpts _ghcInfo _buildDir _cabalCfg _plan _env _pkg _unit _cn _ci =
    return mempty

-------------------------------------------------------------------------------
-- Test component without
-------------------------------------------------------------------------------

testComponentNo
    :: forall r. TracerPeu r Tr
    -> TracerPeu r Tr
    -> (DynOpts -> DynOpts)
    -> GhcInfo
    -> Cabal.Config Identity
    -> Map UnitId IPI.InstalledPackageInfo
    -> Package
    -> Peu r Summary
testComponentNo tracer0 tracerTop dynOptsCli ghcInfo cabalCfg dbG pkg = do
    traceApp tracerTop $ TraceComponent (C.packageId (pkgGpd pkg)) Plan.CompNameLib

    -- use default values for flags
    let flags :: Map C.FlagName Bool
        flags = Map.fromList
            [ (C.flagName flag, C.flagDefault flag)
            | flag <- C.genPackageFlags (pkgGpd pkg)
            ]

    -- "configure"
    lib0 <- maybe (die tracerTop "no library component in GPD") return
        $ C.condLibrary $ pkgGpd pkg
    let (_, lib) = simplifyCondTree ghcInfo flags lib0
    let bi = C.libBuildInfo lib

    -- append options in .cabal file
    dynOptsBi <- dynOptsFromBuildInfo tracerTop bi
    let dynOpts = dynOptsCli (dynOptsBi defaultDynOpts)

    -- Once dynOpts is read we can read adjust verbosity of our tracer
    let tracer = adjustTracer (optVerbosity dynOpts) tracer0

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
    extraUnitIds <- traverse findUnit $ Set.toList $ propPkgs dynOpts <> optExtraPkgs dynOpts

    let pkgIds :: [PackageIdentifier]
        pkgIds = map snd depends

    let unitIds :: [UnitId]
        unitIds = ordNub $ map fst $
            thisUnitId : depends ++ extraUnitIds

    -- find library module paths
    modulePaths <- findModules
        tracer
        (pkgDir pkg)
        (C.hsSourceDirs bi)
        (C.exposedModules lib)

    -- cpp include dirs
    cppDirs <- traverse makeAbsolute (optCppIncludeDirs dynOpts)

    -- first phase: read modules and extract the comments
    modules <- for modulePaths $ \(modname, modpath) ->
        phase1 tracer ghcInfo (pkgDir pkg) cppDirs pkgIds bi modname modpath

    -- extract doctests from the modules.
    let parsed :: [Module [Located DocTest]]
        parsed = fmap4 (doctestStripComments (optStripComs dynOpts))
            $ parseModules modules where

    if optPhase dynOpts > Phase1
    then do
        -- Note: we don't pass additional environment
        -- For non-cabal-plan setup we simply don't support data-files.
        phase2 tracer dynOpts unitIds ghcInfo Nothing cabalCfg (pkgDir pkg) [] parsed
    else
        return $ foldMap skipModule parsed

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

propPkgs :: DynOpts -> Set PackageName
propPkgs dynOpts = case optProperties dynOpts of
    SkipProperties  -> mempty
    CheckProperties -> Set.singleton (mkPackageName "QuickCheck")

manglePackageName :: C.PackageName -> String
manglePackageName = map fixchar . prettyShow where
    fixchar :: Char -> Char
    fixchar '-' = '_'
    fixchar c   = c

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
